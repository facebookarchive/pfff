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
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * history:
 *  - was in check_module.ml
 *  - was generalized for scheck php
 *  - introduced ranking via int (but mess)
 *  - introduced simplified ranking using intermediate rank type
 *  - fully generarlize when introduced graph_code_checker.ml
 * 
 * todo:
 *  - priority to errors, so dead code func more important than dead field
 *  - factorize code with errors_cpp.ml, errors_php.ml, error_php.ml
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
  loc: Parse_info.token_location;
  sev: severity;
}
 (* todo? Advice | Noisy | Meticulous ? *)
 and severity = Fatal | Warning

 and error_kind =
  (* entities *)
   (* done while building the graph:
    *  - UndefinedEntity (UseOfUndefined)
    *  - MultiDefinedEntity (DupeEntity)
    *)
 (* As done by my PHP global analysis checker.
  * Never done by compilers, and unusual for linters to do that.
  *  
  * note: OCaml 4.01 now does that partially by locally checking if 
  * an entity is unused and not exported (which does not require
  * global analysis)
  *)
 | Deadcode of (string * Database_code.entity_kind)
 | UndefinedDefOfDecl of (string * Database_code.entity_kind)

  (* call sites *)
   (* should be done by the compiler (ocaml does):
    * - TooManyArguments, NotEnoughArguments
    * - WrongKeywordArguments
    * - ...
    *)

  (* variables *)
   (* also done by some compilers (ocaml does):
    * - UseOfUndefinedVariable
    * - UnusedVariable
    *)

  (* classes *)

  (* files (include/import) *)

  (* bail-out constructs *)
   (* a proper language should not have that *)

  (* lint *)

  (* other *)



type rank =
 (* Too many FPs for now. Not applied even in strict mode. *)
 | Never
 (* Usually a few FPs or too many of them. Only applied in strict mode. *)
 | OnlyStrict
 | Less
 | Ok
 | Important
 | ReallyImportant

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind error_kind =
  match error_kind with
  | Deadcode (s, kind) -> 
    spf "dead %s, %s" (Database_code.string_of_entity_kind kind) s
  | UndefinedDefOfDecl (s, kind) ->
    spf "no def found for %s (%s)" s (Database_code.string_of_entity_kind kind)

(*
let loc_of_node root n g =
  try 
    let info = G.nodeinfo n g in
    let pos = info.G.pos in
    let file = Filename.concat root pos.PI.file in
    spf "%s:%d" file pos.PI.line
  with Not_found -> "NO LOCATION"
*)

let string_of_error err =
  let pos = err.loc in
  spf "%s:%d: %s" pos.PI.file pos.PI.line (string_of_error_kind err.typ)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let g_errors = ref []

let fatal loc err =
  Common.push { loc = loc; typ = err; sev = Fatal } g_errors
let warning loc err = 
  Common.push { loc = loc; typ = err; sev = Warning } g_errors

(*****************************************************************************)
(* Ranking *)
(*****************************************************************************)

let score_of_rank = function
  | Never -> 0
  | OnlyStrict -> 1
  | Less -> 2
  | Ok -> 3
  | Important -> 4
  | ReallyImportant -> 5

let rank_of_error err =
  match err.typ with
  | Deadcode (_s, kind) ->
      (match kind with
      | E.Function -> Important
      | _ -> Ok
      )
  | UndefinedDefOfDecl _ -> Important
  

let score_of_error err =
  err +> rank_of_error +> score_of_rank

(*****************************************************************************)
(* False positives *)
(*****************************************************************************)

let adjust_errors xs =
  xs +> Common.exclude (fun err ->
    let file = err.loc.PI.file in
    
    match err.typ with
    | Deadcode (s, kind) ->
       (match kind with
       | E.Dir | E.File -> true
       (* FP in graph_code_clang for now *)
       | E.Type when s =~ "E__anon" -> true
       | E.Type when s =~ "E__" -> true
       | E.Type when s =~ "T__" -> true
        
       | E.Prototype | E.GlobalExtern -> true

       (* todo: to remove, but too many for now *)
       | E.Constructor | E.Field -> true
       | _ when file =~ "^include/" -> true
       | _ -> false
       )
    | UndefinedDefOfDecl _ -> file =~ "^include/"
  )


