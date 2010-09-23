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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Centralize PHP errors report functions.
 * 
 * TODO: move more of the code of lint_php.mli here
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let strict = ref false

type error = 
  | TooManyArguments   of (Common.parse_info * name (* def *))
  | NotEnoughArguments of (Common.parse_info * name (* def *))
  | UseOfUndefinedVariable of Ast_php.dname
  | UseOfUndefinedMember of Ast_php.name
  | UnusedVariable of Ast_php.dname * Scope_php.phpscope


exception Error of error

let string_of_error error = 
  let spos info = 
    (* emacs compile-mode compatible output *)
    spf "%s:%d:%d: " info.file info.line info.column
  in
  (match error with
  | TooManyArguments (info, name) ->
      (* TODO use name  ? *)
      (spos info ^ "CHECK: too many arguments");
  | NotEnoughArguments (info, name) ->
      (* TODO use name  ? *)
      (spos info ^ "CHECK: not enough arguments");
  | UseOfUndefinedVariable (dname) ->
      let s = Ast.dname dname in
      let info = Ast.info_of_dname dname |> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: use of undefined variable $%s" s

  | UseOfUndefinedMember (name) ->
      let s = Ast.name name in
      let info = Ast.info_of_name name |> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: use of undefined member $%s" s
  | UnusedVariable (dname, scope) ->
      let s = Ast.dname dname in
      let info = Ast.info_of_dname dname |> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: unused %s variable $%s" 
              (Scope_php.s_of_phpscope scope)
              s 
  )

let report_error err = 
  pr2 (string_of_error err)

let _errors = ref []

(* todo? let exn_when_error *)

let fatal err =
  Common.push2 err _errors

let warning err = 
  Common.push2 err _errors

let report_all_errors () = 
  !_errors |> List.rev |> List.iter report_error
