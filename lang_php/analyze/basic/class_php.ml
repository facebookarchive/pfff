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

(* see also check_module.ml and the places where we call checkClassName *)
let static_new_or_extends_of_ast idast = 

  V2.do_visit_with_ref (fun aref ->
    { V.default_visitor with

    V.kexpr = (fun (k, bigf) x ->
      match Ast.untype x with
      | New (tok, classname_ref, _) 
      | AssignNew (_, _, _, tok, classname_ref, _) ->

          (match classname_ref with
          | ClassNameRefStatic name -> 
              Common.push2 name aref
          | ClassNameRefDynamic _ -> 
              (* can't do much for now *)
              ()
          );
          k x
            
      | _ -> k x
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

    V.kclass_def = (fun (k, _) def ->
      k def;
      def.c_extends +> Common.do_option (fun (tok, classname) ->
        Common.push2 classname aref;
      );
    );
    })
    (fun visitor -> visitor.V2.vid_ast idast)

(* This is used in check_variables_php.ml to allow inherited
 * visible variables to be used in scope
 *)
let get_public_or_protected_vars_of_class def =

  def.c_body +> Ast.unbrace +> Common.map_filter (function
  |  ClassVariables (modifiers, class_vars, _tok) ->
       
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
