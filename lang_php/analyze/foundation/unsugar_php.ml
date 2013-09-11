(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Ast_php

module Ast = Ast_php
module V = Visitor_php
module M = Map_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * There are a few constructions in the PHP AST such as self:: and parent::
 * that makes certain analysis more tedious to write. The goal of this
 * module is just to unsugar those features.
 *
 * If you want a really unsugared AST you should use pil.ml.
 *
 * note that even if people use self::foo(), the foo() method may
 * actually not be in self but possibly in its parents; so we need
 * a lookup ancestor anyway ...
 *
 * todo? turns out people also use self:: or parent:: or static::
 * in strings, to pass callbacks, so may have to unsugar the strings
 * too?
 *
 * todo: have a unsugar_traits() that do the inlining of the mixins.
 * This requires an entity_finder.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* I also return the original token of self/parent so the caller can decide
 * to do a rewrap on it. This is better than subsituting
 * the name by the referenced class because ii_of_any and range_of_ii
 * could get confused by having some ASTs that contains "foreign" ii.
 *)
let resolve_class_name qu in_class =
  match qu, in_class with
  | (Self (tok1)), Some (name, _parent) ->
      name, tok1
  | (Parent (tok1)), (Some (_, Some parent)) ->
      parent, tok1
  | (Self (tok1)), None ->
      (* I used to failwith, but our codebase contains such crap
       * and we don't want all of our analysis to fail on one file
       * just because of those wrong self/parent. Turns them
       * into regular unknown class so get the same benefits.
       *)
      pr2 ("PB: Use of self:: outside of a class");
      Name ("UnkwnownUseOfSelf", tok1), tok1
  | (Parent (tok1)), _ ->
      pr2 "PB: Use of parent:: in a class without a parent";
      Name ("UnkwnownUseOfParent", tok1), tok1
  (* this should never be reached, the caller will special case LateStatic
   * before calling resolve_class_name
   *)
  | (LateStatic tok1), _ ->
      failwith "LateStatic"
  | _ -> raise Impossible

let contain_self_or_parent def =
  let aref = ref false in
  let visitor = V.mk_visitor { V.default_visitor with
    V.kname = (fun (k, bigf) qu ->
      match qu with
      | Self _ | Parent _ -> aref := true
      | LateStatic _ | XName _ -> ()
    );
    }
  in
  visitor (Toplevel (ClassDef def));
  !aref

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let unsugar_self_parent_any2 any =

  (* dupe: this is also done in check_module/uses_module.ml *)
  let in_class = ref (None: (Ast.ident * Ast.ident option) option) in

  let visitor = M.mk_visitor ({ M.default_visitor with

    M.kclass_def = (fun (k, _) def ->
      let classname = def.c_name in
      let parent_opt =
        match def.c_extends with
        | None -> None
        | Some (tok, (Hint (XName [QI classname], _targs))) -> Some classname
(*
        | Some (tok, (Hint (XName elts, _targs))) ->
            let names = List.map (function QI x -> str_of_ident x | QITok tok -> "\\") elts in
            let namestr = String.concat "" names in
            Some (Name (namestr, fakeInfo namestr))
*)
        | Some x -> 
          failwith ("Warning: unknown extends clause\n" ^
                       (Export_ast_ml.string_of_v 
                          (Meta_ast_php.vof_any (Hint2 (snd x)))))
      in

      match def.c_type with
      (* Some traits contain reference to parent:: which we can not
       * unsugar at the defition location. We can do such thing
       * only at the 'use' location. So let's skip the transformation
       * of the trait definition here and not call the continuation k.
       *)
      | Trait _ ->
          def
      | _ ->
         Common.save_excursion in_class (Some (classname, parent_opt)) (fun ()->
           k def
         )
    );

    M.kname = (fun (k, bigf) qu ->
      match qu with
      | LateStatic tok -> LateStatic tok
      | XName name -> XName name
      | Self _ | Parent _ ->
          let (unsugar_name, tok_orig) =
            resolve_class_name qu !in_class in
          let name' =
            match unsugar_name with
            | Name (s, _info_of_referenced_class) ->
                Name (s, tok_orig)
            | XhpName (xs, _info_of_referenced_class) ->
                XhpName (xs, tok_orig)
          in
          XName [QI (name')]
    );
  })
  in
  visitor.M.vany any

let unsugar_self_parent_any a =
  Common.profile_code "Unsugar_php.self_parent" (fun () ->
    unsugar_self_parent_any2 a)

(* special case *)
let unsugar_self_parent_program ast =
  unsugar_self_parent_any (Program ast) +>
    (function Program x -> x | _ -> raise Impossible)
