(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

module E = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *  - priority to errors, so dead code func more important than dead field
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* see g_errors below *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type error = {
  typ: error_kind;
  loc: Parse_info.info;
  sev: severity;
}
 (* todo? Advice | Noisy | Meticulous ? *)
 and severity = Fatal | Warning

 and error_kind =
 (* done while building the graph:
  *  - UseOfUndefinedEntity
  *  - DupeEntity
  *)

 (* As done by my PHP global analysis checker.
  *  
  * ocaml 4.01 now do that partially by locally checking if 
  * unused and not exported (which does not require global analysis)
  *)
 | Deadcode of Database_code.entity_kind

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind error_kind =
  match error_kind with
  | Deadcode kind -> spf "dead %s" (Database_code.string_of_entity_kind kind)

let string_of_error _error =
  raise Todo

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let g_errors = ref []

let fatal loc err =
  Common.push { loc = loc; typ = err; sev = Fatal } g_errors
let warning loc err = 
  Common.push { loc = loc; typ = err; sev = Warning } g_errors
