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

open Ast_php

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Mimic Entity.id_kind. TODO move in entity_php.ml ?
 *)
type id_ast = 
  | Function of func_def
  | Class of class_def
  | Interface of interface_def
  | StmtList of stmt list

  | Method of method_def

  (* todo: may want one id_ast per actual entity, so ClassVariable not
   * ClassVariable_s_
   *)
  | ClassConstant of class_constant
  | ClassVariable of class_variable * modifier list
  | XhpDecl of xhp_decl

  | Misc of info list
 (* with tarzan *)



let toplevel_to_idast x = 
  match x with
  | Ast.StmtList v1 -> 
      StmtList v1
  | FuncDef v1 ->
      Function v1
  | ClassDef v1 -> 
      Class v1
  | InterfaceDef v1 -> 
      Interface v1

  (* todo? *)
  | Halt ((v1, v2, v3)) ->
      Misc []
  | NotParsedCorrectly xs ->
      Misc xs
  | FinalDef v1 ->
      Misc [v1]


type entity_finder = (Entity_php.id_kind * string) -> id_ast list
