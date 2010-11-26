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
 * Centralize PHP errors report functions (they did the same in c--)
 * 
 * TODO: move more of the code of lint_php.mli here
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let strict = ref false

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* the position in the name below correspond to the function at the call site 
 * coupling: if you add a constructor here, don't forget to extend
 * layer_checker_php.ml too
 *)
type error = 
  (* functions *)
  | UndefinedFunction of Ast_php.name
  | UnableToDetermineDef of Ast_php.name

  (* call sites *)
  | TooManyArguments   of (Parse_info.parse_info * name (* def *))
  | NotEnoughArguments of (Parse_info.parse_info * name (* def *))
  | TooManyArguments2 of Ast_php.name * Ast_php.func_def
  | TooFewArguments2  of Ast_php.name * Ast_php.func_def
  | WrongKeywordArgument of Ast_php.dname * Ast_php.expr * Ast_php.name *
                     Ast_php.parameter * Ast_php.func_def

  (* variables *)
  | UseOfUndefinedVariable of Ast_php.dname
  | UnusedVariable of Ast_php.dname * Scope_php.phpscope

  (* classes *)
  | UseOfUndefinedMember of Ast_php.name

  (* bail-out constructs *)
  | UglyGlobalDynamic of Ast_php.info
  | WeirdForeachNoIteratorVar of Ast_php.info

  (* cfg, mostly DeadCode statements *)
  | CfgError of Controlflow_build_php.error

exception Error of error

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error error = 
  let spos info = 
    (* emacs compile-mode compatible output *)
    spf "%s:%d:%d: " 
      info.Parse_info.file info.Parse_info.line info.Parse_info.column
  in
  match error with
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

  | UndefinedFunction(funcname) ->
      (spf "Warning: at %s
  function %s is undefined"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
      )
  | UnableToDetermineDef(funcname) ->
      (spf "Warning: at %s
  function %s is defined several times; unable to find which one applies"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
      )
  | WrongKeywordArgument(dn, expr, funcname, param, def) ->
      (spf "Warning: at %s
  the assignment '$%s=%s' in the argument list of this call to '%s()' looks like a keyword argument, but the corresponding parameter in the definition of '%s' is called '$%s'
  function was declared: %s"
          (Ast.string_of_info (Ast.info_of_dname dn))
          (Ast.dname dn) (Unparse_php.string_of_expr expr)
          (Ast.name funcname) (Ast.name funcname)
          (Ast.dname param.p_name)
          (Ast.string_of_info (Ast.info_of_name def.f_name))
      )
  | TooManyArguments2(funcname, def) ->
      (spf "Warning: at %s
  function call %s has too many arguments
  function was declared: %s"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
          (Ast.string_of_info (Ast.info_of_name def.f_name))
      )
  | TooFewArguments2(funcname, def) ->
      (spf "Warning: at %s
  function call %s has too few arguments
  function was declared: %s"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
          (Ast.string_of_info (Ast.info_of_name def.f_name))
      )

  | UglyGlobalDynamic info ->
      let pinfo = Ast.parse_info_of_info info in
      spos pinfo ^ "CHECK: ugly dynamic global declaration"
  | WeirdForeachNoIteratorVar info ->
      let pinfo = Ast.parse_info_of_info info in
      spos pinfo ^ "CHECK: weird, foreach with not a var as iterator"

  | CfgError err ->
      Controlflow_build_php.string_of_error err
  
        
let info_of_error err =
  match err with
  | UndefinedFunction name 
  | UnableToDetermineDef name

  | TooManyArguments2 (name, _)
  | TooFewArguments2  (name, _)
      -> Some (Ast.info_of_name name)

  | TooManyArguments  (parse_info, name (* def *)) ->
      raise Todo
  | NotEnoughArguments (parse_info, name (* def *)) ->
      raise Todo
  | WrongKeywordArgument (dname,  expr, name, param, fdef) ->
      raise Todo

  | UseOfUndefinedVariable dname
  | UnusedVariable (dname, _)
      -> Some (Ast.info_of_dname dname)

  | UseOfUndefinedMember name 
      -> Some (Ast.info_of_name name)

  | UglyGlobalDynamic info
  | WeirdForeachNoIteratorVar info
      -> Some info

  | CfgError err ->
      Controlflow_build_php.info_of_error err

let report_error err = 
  pr2 (string_of_error err)

(*****************************************************************************)
(* Global bis *)
(*****************************************************************************)

let _errors = ref []

(* todo? let exn_when_error *)

let fatal err =
  Common.push2 err _errors
(* no difference for now ... *)
let warning err = 
  Common.push2 err _errors

let report_all_errors () = 
  !_errors |> List.rev |> List.iter report_error
