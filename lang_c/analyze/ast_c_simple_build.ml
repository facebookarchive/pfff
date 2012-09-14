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
exception TodoConstruct of string * Ast_cpp.info

(* not used for now *)
type env = unit

let empty_env () = ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let todo any =
  let _ii = Lib_parsing_cpp.ii_of_any any in
  pr2 (Export_ast_cpp.ml_pattern_string_of_any any);
  raise Todo


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program x =
  let env = empty_env () in

  List.map (toplevel env) x +> List.flatten

and toplevel env = function
  | CppTop x -> cpp_directive env x

  | FinalDef _ -> []
  (* not much we can do here, at least the parsing statistics should warn the
   * user that some code was not processed
   *)
  | NotParsedCorrectly _ -> []

  | x -> todo (Toplevel x)

and cpp_directive env = function
  | Define (tok, name, def_kind, def_val) as x ->
      (match def_kind, def_val with
      | DefineVar, DefineExpr e ->
          [A.Define (name, expr env e)]
      | _ -> todo (Cpp x)
      )
  | x -> todo (Cpp x)


and expr env e =
  let (e', toks) = e in
  match e' with
  | C cst -> constant env toks cst
  | _ -> todo (Expr e)

and constant env toks x = 
  match x, toks with
  | Int s, [x] -> A.Int (s,x)
  | _ -> todo (Constant x)

