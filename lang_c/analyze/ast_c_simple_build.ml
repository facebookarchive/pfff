(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

open Ast_cpp
module A = Ast_c_simple

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Ast_cpp to Ast_c_simple.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

exception ObsoleteConstruct of string * Ast_cpp.info
exception CplusplusConstruct
exception TodoConstruct of string * Ast_cpp.info

(* not used for now *)
type env = unit

let empty_env () = ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec debug any =
  let _ii = Lib_parsing_cpp.ii_of_any any in
  pr2 (Export_ast_cpp.ml_pattern_string_of_any any)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program x =
  let env = empty_env () in
  List.map (toplevel env) x +> List.flatten

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevel env = function
  | CppTop x -> cpp_directive env x

  | Func (func_or_else) as x ->
      (match func_or_else with
      | FunctionOrMethod def ->
          [func_def env def]
      | Constructor _ | Destructor _ ->
          debug (Toplevel x); raise CplusplusConstruct
      )

  | EmptyDef _ -> []
  | FinalDef _ -> []

  | NameSpaceAnon (_, _)|NameSpaceExtend (_, _)|NameSpace (_, _, _) 
  | ExternCList (_, _, _)|ExternC (_, _, _)|TemplateSpecialization (_, _, _)
  | TemplateDecl _
      as x ->
      debug (Toplevel x); raise CplusplusConstruct
      
  | (MacroVarTop (_, _)|MacroTop (_, _, _)|IfdefTop _|BlockDecl _|DeclTodo) 
      as x ->
      debug (Toplevel x);
      raise Todo

  (* not much we can do here, at least the parsing statistics should warn the
   * user that some code was not processed
   *)
  | NotParsedCorrectly _ -> []

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_def env def =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Cpp *)
(* ---------------------------------------------------------------------- *)
  
and cpp_directive env = function
  | Define (tok, name, def_kind, def_val) as x ->
      (match def_kind, def_val with
      | DefineVar, DefineExpr e ->
          [A.Define (name, expr env e)]
      | _ -> debug (Cpp x); raise Todo
      )
  | Include (tok, inc_file) as x ->
      let s =
        (match inc_file with
        | Local xs -> "\"" ^ Common.join "/" xs ^ "\""
        | Standard xs -> "<" ^ Common.join "/" xs ^ ">"
        | Wierd s -> 
            debug (Cpp x); raise Todo
        )
      in
      [A.Include (s, tok)]
  | (PragmaAndCo _|Undef _) as x ->
      debug (Cpp x); raise Todo

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

and stmt env x =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

and expr env e =
  let (e', toks) = e in
  match e' with
  | C cst -> constant env toks cst
  | (TypeIdOfType (_, _)|TypeIdOfExpr (_, _)|GccConstructor (_, _)
  | StatementExpr _|
Cast (_, _)|SizeOfType (_, _)|SizeOfExpr (_, _)|RecordPtStarAccess (_, _)|
RecordStarAccess (_, _)|RecordPtAccess (_, _)|RecordAccess (_, _)|
ArrayAccess (_, _)|Binary (_, _, _)|Unary (_, _)|Infix (_, _)|Postfix (_, _)|
Assignment (_, _, _)|Sequence (_, _)|CondExpr (_, _, _)|FunCallExpr (_, _)|
FunCallSimple (_, _)|Ident (_, _)|ExprTodo) ->
      debug (Expr e); raise Todo

  | Throw _|DeleteArray (_, _)|Delete (_, _)|New (_, _, _, _, _)
  | CplusplusCast (_, _, _)
  | ConstructedObject (_, _) | This _
      ->
      debug (Expr e); raise CplusplusConstruct

  | ParenExpr (_, e, _) -> expr env e

and constant env toks x = 
  match x, toks with
  | Int s, [x] -> A.Int (s,x)
  | (Bool _|Float _|Char _|String _|MultiString), _ -> 
      debug (Constant x); raise Todo
  | _, _ -> 
      debug (Constant x); raise Impossible

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)



