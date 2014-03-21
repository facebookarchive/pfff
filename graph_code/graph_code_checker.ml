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

module E = Database_code
module G = Graph_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *  - priority to errors, so dead code func more important than dead field
 * 
 * less: could move error code in specific graph_code_checker_error
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

let loc_of_node root n g =
  try 
    let info = G.nodeinfo n g in
    let pos = info.G.pos in
    let file = Filename.concat root pos.PI.file in
    spf "%s:%d" file pos.PI.line
  with Not_found -> "NO LOCATION"

(*****************************************************************************)
(* Checker *)
(*****************************************************************************)
let check root g =

  let pred = G.mk_eff_use_pred g in

  g +> G.iter_nodes (fun n ->
    let (s, kind) = n in
    let file =
      try G.file_of_node n g
      with Not_found -> "NotFound"
    in

    let ps = pred n in
    (* todo: filter nodes that are in boilerplate code *)
    if ps = [] then begin
      (match kind with
      | E.Dir | E.File -> ()
      (* FP in graph_code_clang for now *)
      | E.Type when s =~ "E__anon" -> ()
      | E.Type when s =~ "E__" -> ()
      | E.Type when s =~ "T__" -> ()
        
      | E.Prototype | E.GlobalExtern -> ()

      (* todo: to remove, but too many for now *)
      | E.Constructor | E.Field -> ()

      | _ when file =~ "^include/" -> ()
      | _ ->
        pr2 (spf "%s: %s Dead code?" 
               (loc_of_node root n g)
               (G.string_of_node n)
               )
      )
    end
  )
