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

module E = Entity_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Centralize errors report functions (they did the same in c--).
 * Mostly a copy paste of error_php.ml
 * 
 * history:
 *  - was in check_module.ml
 *  - was generalized for scheck php
 *  - introduced ranking via int (but mess)
 *  - introduced simplified ranking using intermediate rank type
 *  - fully generalize when introduced graph_code_checker.ml
 *  - added @Scheck annotation
 *  - added some false positive deadcode detection
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
 (* less: Advice | Noisy | Meticulous ? *)
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
 | Deadcode of entity
 | UndefinedDefOfDecl of entity
 (* really a special case of Deadcode decl *)
 | UnusedExport of entity (* tge decl*) * Common.filename (* file of def *)

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
  | UnusedVariable of string * Scope_code.scope

  (* classes *)

  (* files (include/import) *)

  (* bail-out constructs *)
   (* a proper language should not have that *)

  (* lint *)

  (* other *)

 (* todo: should be merged with Graph_code.entity or put in Database_code?*)
 and entity = (string * Entity_code.entity_kind)


type rank =
 (* Too many FPs for now. Not applied even in strict mode. *)
 | Never
 (* Usually a few FPs or too many of them. Only applied in strict mode. *)
 | OnlyStrict
 | Less
 | Ok
 | Important
 | ReallyImportant

(* @xxx to acknowledge or explain false positives *)
type annotation =
  | AtScheck of string

(* to detect false positives (we use the Hashtbl.find_all property) *)
type identifier_index = (string, Parse_info.token_location) Hashtbl.t

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind error_kind =
  match error_kind with
  | Deadcode (s, kind) -> 
    spf "dead %s, %s" (Entity_code.string_of_entity_kind kind) s
  | UndefinedDefOfDecl (s, kind) ->
    spf "no def found for %s (%s)" s (Entity_code.string_of_entity_kind kind)
  | UnusedExport ((s, kind), file_def) ->
    spf "useless export of %s (%s) (consider forward decl in %s)" 
      s (Entity_code.string_of_entity_kind kind) file_def

  | UnusedVariable (name, scope) ->
      spf "Unused variable %s, scope = %s" name 
        (Scope_code.string_of_scope scope)

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
(* Main entry points *)
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
      | E.Function -> Ok
      (* or enable when use propagate_uses_of_defs_to_decl in graph_code *)
      | E.GlobalExtern | E.Prototype -> Less
      | _ -> Ok
      )
  (* probably defined in assembly code? *)
  | UndefinedDefOfDecl _ -> Important
  (* we want to simplify interfaces as much as possible! *)
  | UnusedExport _ -> ReallyImportant
  | UnusedVariable _ -> Less
  

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

       (* kencc *)
       | E.Prototype when s = "SET" || s = "USED" -> true

       (* FP in graph_code_clang for now *)
       | E.Type when s =~ "E__anon" -> true
       | E.Type when s =~ "U__anon" -> true
       | E.Type when s =~ "S__anon" -> true
       | E.Type when s =~ "E__" -> true
       | E.Type when s =~ "T__" -> true

       (* FP in graph_code_c for now *)
       | E.Type when s =~ "U____anon" -> true
        
       (* TODO: to remove, but too many for now *)
       | E.Constructor
       | E.Field 
         -> true

       (* hmm plan9 specific? being unused for one project does not mean
        * it's not used by another one.
        *)
       | _ when file =~ "^include/" -> true

       | _ when file =~ "^EXTERNAL/" -> true

       (* too many FP on dynamic lang like PHP *)
       | E.Method -> true

       | _ -> false
       )

    (* kencc *)
    | UndefinedDefOfDecl (("SET" | "USED"), _) -> true

    | UndefinedDefOfDecl _ -> 

      (* hmm very plan9 specific *)
      file =~ "^include/" ||
      file = "kernel/lib/lib.h" ||
      file = "kernel/network/ip/ip.h" ||
      file =~ "kernel/conf/" ||
      false

    | _ -> false
  )

(*****************************************************************************)
(* Annotations *)
(*****************************************************************************)

let annotation_of_line_opt s =
  if s =~ ".*@\\([A-Za-z_]+\\):[ ]?\\([^@]*\\)"
  then
    let (kind, explain) = Common.matched2 s in
    Some (match kind with
      | "Scheck" -> AtScheck explain
      | s -> failwith ("Bad annotation: " ^ s)
    )
  else None

(* The user can override the checks by adding special annotations
 * in the code at the same line than the code it related to.
 *)
let annotation_at2 loc =
  let file = loc.PI.file in
  let line = max (loc.PI.line - 1) 1 in
  match Common2.cat_excerpts file [line] with
  | [s] -> annotation_of_line_opt s
  | _ -> failwith (spf "wrong line number %d in %s" line file)

let annotation_at a =
  Common.profile_code "Errors_code.annotation" (fun () -> annotation_at2 a)
