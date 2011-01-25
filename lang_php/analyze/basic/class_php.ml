(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
module V2 = Visitor2_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let constructor_name = "__construct"

(*****************************************************************************)
(* Ast Helpers *)
(*****************************************************************************)

(* see also check_module.ml and the places where we call checkClassName 
 * todo: what with self:: and parent:: ? have to hook earlier than
 *  kfully_qualified_class_name, in kqualifier. Or just make a 
 *  pass that remove this sugar so then have a simpler AST and
 *  can raise Impossible for Parent and Self cases.
 *)
let users_of_class_in_ast idast = 

  V2.do_visit_with_ref (fun aref ->
    { V.default_visitor with

      (* this covers the new X and instanceof X  *)
      V.kclass_name_reference = (fun (k, bigf) classname_ref ->
        (match classname_ref with
        | ClassNameRefStatic name -> 
            Common.push2 name aref
        | ClassNameRefDynamic _ -> 
            (* can't do much for now *)
            ()
        );
        k classname_ref
      );
      (* this covers the X::, extends X, catch(X) *)
      V.kfully_qualified_class_name = (fun (k, bigf) classname ->
        Common.push2 classname aref;
        k classname
      );

      (* xhp: there is also implicitely a new when we use a XHP tag *)
      V.kxhp_html = (fun (k, _) x ->
        match x with
        | Xhp (xhp_tag, _attrs, _tok, _body, _end) ->
            Common.push2 (XhpName xhp_tag) aref;
            k x
        | XhpSingleton (xhp_tag, _attrs, _tok) ->
            Common.push2 (XhpName xhp_tag) aref;
            k x
      );
    })
    (fun visitor -> visitor.V2.vid_ast idast)



(* This is used in check_variables_php.ml to allow inherited
 * visible variables to be used in scope
 *)
let get_public_or_protected_vars_of_class def =

  def.c_body +> Ast.unbrace +> Common.map_filter (function
  |  ClassVariables (modifiers, _opt_ty, class_vars, _tok) ->
       
       let modifiers = Ast.unmodifiers modifiers in
       
       if List.mem Public modifiers ||
          List.mem Protected modifiers
       then
         let dnames = 
           class_vars |> Ast.uncomma |> List.map fst
         in
         Some dnames
       else None

  | (XhpDecl _|Method _|ClassConstants (_, _, _)) ->
      (* could maybe do something with XhpDecl ? *)
      None
  ) +> List.flatten



let get_constructor def =
  def.c_body +> Ast.unbrace +> Common.find_some (fun class_stmt ->
    match class_stmt with
    | Method def when 
          Ast.name def.m_name = constructor_name ->
        Some def
    | _ -> None
  )


let class_variables_reorder_first def = 
  let (lb, body, rb) = def.c_body in
  let body' =
    let (vars, rest) = 
      body +> List.partition (function
      | ClassVariables _ -> true
      | _ -> false
      )
    in
    vars ++ rest
  in
  { def with
    c_body = (lb, body', rb);
  }

let is_static_method def =
  raise Todo
