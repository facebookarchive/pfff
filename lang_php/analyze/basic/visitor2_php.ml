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

module Ast = Ast_php 
module V = Visitor_php

open Ast_php

module Ast2 = Ast_entity_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Can define here extra visitors, like for control flow, or the 
 * types in ast_entity_php 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type visitor_out = {
  vorigin: Visitor_php.visitor_out;
  vid_ast: Ast_entity_php.id_ast -> unit;
}

let default_visitor = V.default_visitor

(*****************************************************************************)
(* Builder *)
(*****************************************************************************)

(* TODO? call it mk_visitor_no_visit_nested_id ?  *)
let (mk_visitor: V.visitor_in -> visitor_out) = fun vin ->

  (* horrible hack *)
  let is_toplevel_class_member = ref false in

  let hooks = { vin with


    (* Do not visit parts of code that will be analyzed anyway later because
     * it corresponds to another entity. See Database_php_build.index_db2
     *)
    V.kstmt_and_def = (fun (k, bigf) x ->
      match x with
      | Stmt x -> vin.V.kstmt_and_def (k, bigf) (Stmt x)
      | FuncDefNested _
      | ClassDefNested _ 
      | InterfaceDefNested _
          -> 
          (* Not visiting nested stuff. Will be done for another id. *)
          ()
    );
    V.kclass_stmt = (fun (k, bigf) x ->
      match x with
      | Ast.Method _ 
      | Ast.ClassConstants _
      | Ast.ClassVariables _
      | Ast.XhpDecl _
        ->
          if !is_toplevel_class_member
          then begin 
            is_toplevel_class_member := false;
            vin.V.kclass_stmt (k, bigf) x
          end
          else 
            (* Not visiting class members. Will be done for another id. *)
            ()
    );
  }
  in

  let vout_origin_patched = V.mk_visitor hooks in

  let v_id_ast x = 
    match x with
    | Ast2.Function v1 -> 
        vout_origin_patched (Toplevel (FuncDef v1))
    | Ast2.Class v1 -> 
        vout_origin_patched (Toplevel (ClassDef v1))
    | Ast2.Interface v1 -> 
        vout_origin_patched (Toplevel (InterfaceDef v1))
    | Ast2.StmtList v1 -> 
        vout_origin_patched (Toplevel (StmtList v1))


    | Ast2.Method v1 -> 
        is_toplevel_class_member := true;
        vout_origin_patched (ClassStmt (Method v1))

    (* TODO the previous patch hook will forbid any processing 
     * of class members, which we dont want for actual id_ast class 
     * members 
     *)
    | Ast2.ClassConstant class_cst ->
        is_toplevel_class_member := true;
        vout_origin_patched (ClassConstant2 class_cst)

    | Ast2.ClassVariable (class_var, _modifier) ->
        is_toplevel_class_member := true;
        vout_origin_patched (ClassVariable class_var)

    | Ast2.XhpDecl xhp ->
        is_toplevel_class_member := true;
        vout_origin_patched (ClassStmt (XhpDecl xhp))


    | Ast2.Misc xs -> 
        vout_origin_patched (InfoList xs)

  in
  { 
    vorigin = vout_origin_patched;
    vid_ast = v_id_ast;
  }





let do_visit_with_ref mk_hooks recursor = 
  let res = ref [] in
  let hooks = mk_hooks res in
  begin
    let vout = mk_visitor hooks in
    recursor vout;
    !res
  end
