(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

module Ast = Ast_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Centralize errors report functions (they did the same in c--).
 * Mostly a copy paste of error_php.ml
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type error = {
  typ: error_kind;
  loc: Ast_cpp.info;
  sev: severity;
}
 (* todo? Advice | Noisy | Meticulous ? *)
 and severity = Fatal | Warning

 and error_kind = 
  (* entities *)
  (* call sites *)

  (* variables *)
  | UnusedVariable of string * Scope_code.scope

  (* classes *)
  (* bail-out constructs *)
  (* cfg, mostly DeadCode statements *)

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind error_kind =

  match error_kind with
  | UnusedVariable (name, scope) ->
      spf "Unused variable %s, scope = %s" name 
        (Scope_code.string_of_scope scope)

(*****************************************************************************)
(* Global bis *)
(*****************************************************************************)

let _errors = ref []

let fatal loc err =
  Common.push2 { loc = loc; typ = err; sev = Fatal } _errors
let warning loc err = 
  Common.push2 { loc = loc; typ = err; sev = Warning } _errors

(*****************************************************************************)
(* Ranking *)
(*****************************************************************************)
