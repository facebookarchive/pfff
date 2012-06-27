(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2011 Facebook
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
module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * There are many places where we need to get access to the list of
 * entities defined in a file or used in a file (e.g. for tags).
 * 
 * This file is concerned with entities, that is Ast_php.name.
 * For completness C-s for name in ast_php.ml and see if all uses of 
 * it are covered. Other files are more concerned about checks related 
 * to variables, that is Ast_php.dname, as in check_variables_php.ml
 * 
 * todo: factorize code in
 *  - database_php_build.ml
 *  - lib_parsing_php.ml many get_xxx_any ?
 *  - check_module.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the name option is a little bit ugly because it's valid only for
 * nested entities like Method. One could make a specific
 * entity_kind for PHP but this will go against the multi-languages
 * factorization we try to do in h_program-lang/
 *)
type def =
  Database_code.entity_kind * Ast_php.name * Ast_php.name option

type use =
  Database_code.entity_kind * Ast_php.name

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* 
 * todo: similar to what is in database_php_build.ml
 * 
 * history: was previously duplicated in 
 *  - tags_php.ml
 *  - TODO check_module.ml and defs_module.ml
 *  - TODO database_php_build.ml ?
 *)
let defs_of_any any =
  let current_class = ref (None: Ast_php.name option) in

  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kfunc_def = (fun (k, _) def ->
      (match def.f_type with
      | FunctionRegular -> 
          Common.push2 (Db.Function, def.f_name, None) aref
      | MethodRegular | MethodAbstract ->
          (* handled in kmethod_def *)
          ()
      | FunctionLambda ->
          (* the f_name is meaningless *)
          ()
      );
      (* could decide to not recurse, but could have nested function ?
       * hmm they are usually under some toplevel ifs, not inside functions.
       *)
      k def
    );
    V.kclass_def = (fun (k, _) def ->
      let kind = 
        match def.c_type with
        | ClassRegular _ | ClassFinal _ | ClassAbstract _ -> Db.RegularClass
        | Interface _ -> Db.Interface
        | Trait _ -> Db.Trait
      in
      Common.push2 (Db.Class kind, def.c_name, None) aref;
      Common.save_excursion current_class (Some def.c_name) (fun () ->
          k def;
      );
    );

    V.kmethod_def = (fun (k, _) (modifiers, def) ->
      let classname =
        match !current_class with
        | Some c -> c
        | None -> failwith "impossible: no current_class in defs_use_php.ml"
      in
      let kind =
        if Class_php.is_static_method (modifiers, def)
        then Db.StaticMethod
        else Db.RegularMethod
      in
      Common.push2 (Db.Method kind, def.f_name, Some classname) aref;
      k (modifiers, def)
    );
    V.ktop = (fun (k, _) x ->
      match x with
      (* const of php 5.3 *)
      | ConstantDef (tok, name, tok_equal, scalar, semicolon) ->
          Common.push2 (Db.Constant, name, None) aref;
          k x
      | _ -> k x
    );

    V.klvalue = (fun (k, bigf) x ->
      match x with
      | FunCallSimple((Name ("define", tok)), args) ->
          let args = args |> Ast.unparen |> Ast.uncomma in
          (match args with
          (* Maybe better to have a Define directly in the AST. Note that
           * PHP 5.3 has a new const features that makes the use of define
           * obsolete.
           *)
          | (Arg ((Sc (C (String (s,info))))))::xs -> 
              (* by default the info contains the '' or "" around the string,
               * which is not the case for s. See ast_php.ml
               *)
              let info' = Ast.rewrap_str (s) info in
              Common.push2 (Db.Constant, (Name (s, info')), None) aref;
              k x
          | _ -> k x
          )
      | _ -> k x
    );

    (* todo: globals? fields? class constants? *)

  }) any

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)
(* 
 * Cover every cases ? C-s for 'name' in ast_php.ml.
 * update: C-s for 'xhp_tag' too.
 * See unit_foundation_php.ml for the unit tests.
 *
 * history: was previously duplicated in 
 *  - class_php.ml, 
 *  - TODO check_module.ml and uses_module.ml
 * 
 * todo: 
 * - do for constants too ! see Database_code.entity_kind
 * - check_module.ml and the places where we call checkClassName,
 *   same than here ?
 * - todo? right now I don't make a difference between Class and Interface.
 *   If want then just intercept in the clas_def hook the processing
 *   of 'implements'.
 * - todo? todo? do stuff for dynamic stuff like ClassNameRefDynamic ?
 *   return a special Tag ? DynamicStuff ? So at least know they
 *   are connections to more entities than one can infer statically.
 *)
let uses_of_any ?(verbose=false) any = 

  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kqualifier = (fun (k, bigf) x ->
      (match fst x with
      | ClassName _ -> ()
      | Self _ | Parent _ -> 
          if verbose then pr2 "defs_uses_php: call unsugar_self_parent";
          ()
      | LateStatic _ ->
          if verbose then pr2 "LateStatic";
          ()
      );
      k x
    );

    V.klvalue = (fun (k, bigf) x ->
      (match x with
      (* todo: what about functions passed as strings? *)
      | FunCallSimple (name, args) ->
          Common.push2 (Db.Function, name) aref;
      | _ -> ()
      );
      k x
    );

    (* This covers
     * - new X, instanceof X 
     *   (via class_name_reference), 
     * - X::Cst, X::$var, X::method(), X::$f() 
     *   (via qualifier)
     * - extends X, implements X, catch(X), function foo(X $f)
     *   (via fully_qualified_class_name)
     *)
    V.kfully_qualified_class_name = (fun (k, bigf) classname ->
      (* todo? can interface define constant ? in which case
       * there is some ambiguity when seeing X::cst ...
       * could be the use of a Class or Interface.
       * So right now I just merge Class and Interface
       *)
      (* todo?? how know? for new it's always a RegularClass, but for the 
       * rest? 
       *)
      let kind = Db.RegularClass in

      Common.push2 (Db.Class kind, classname) aref;
      k classname
    );

    (* xhp: there is also implicitely a new when we use a XHP tag *)
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp (xhp_tag, _attrs, _tok, _body, _end) ->
          Common.push2 (Db.Class Db.RegularClass, (XhpName xhp_tag)) aref;
          k x
      | XhpSingleton (xhp_tag, _attrs, _tok) ->
          Common.push2 (Db.Class Db.RegularClass, (XhpName xhp_tag)) aref;
          k x
      (* todo: do it also for xhp attributes ? kxhp_tag then ?
       * (but take care to not include doublon because have
       * a xhp_tag for the open tag and one for the close.
       *)
    );
  }) any
