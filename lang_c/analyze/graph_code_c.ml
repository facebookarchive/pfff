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

module E = Database_code
module G = Graph_code

open Ast_c_simple
module Ast = Ast_c_simple

module Flag = Flag_parsing_cpp
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for C. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * todo? reuse code with the other graph_code_xxx ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could maybe call Parse_c.parse to get the parsing
 * statistics
 *)

let parse file =
  try 
    (* less: make this parameters of parse_program? *) 
    Common.save_excursion Flag.error_recovery true (fun () ->
    Common.save_excursion Flag.show_parsing_error false (fun () ->
    Common.save_excursion Flag.verbose_parsing false (fun () ->
    let cst = Parse_c.parse_program file in
    let ast = Ast_c_simple_build.program cst in
    ast
    )))
  with 
  | Timeout -> raise Timeout
  | exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    []

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

let extract_defs ~g ~dupes ~ast ~readable =
  let dir = Common.dirname readable in
  G.create_intermediate_directories_if_not_present g dir;
  g +> G.add_node (readable, E.File);
  g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  raise Todo

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

let rec extract_uses ~g ~ast ~dupes ~readable ~lookup_fails ~skip_edges =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_c.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  g +> G.add_node G.root;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  let dupes = Hashtbl.create 101 in
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse file in
      extract_defs ~g ~dupes ~ast ~readable;
    ));
  dupes +> Common.hashset_to_list +> List.iter (fun n ->
    pr2 (spf "DUPE: %s" (G.string_of_node n));
  );

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep2: extract uses";
  let lookup_fails = Common.hash_with_default (fun () -> 0) in
  let skip_edges = skip_list +> Common.map_filter (function
    | Skip_code.Edge (s1, s2) -> Some (s1, s2)
    | _ -> None
  ) +> Common.hash_of_list 
  in
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse file in
     extract_uses ~g ~dupes ~ast ~readable ~lookup_fails ~skip_edges;
   ));

  lookup_fails#to_list +> Common.sort_by_val_highfirst +> Common.take_safe 20
  +> List.iter (fun (n, cnt) ->
    pr2 (spf "LOOKUP FAIL: %s (%d)" (G.string_of_node n) cnt)
  );
  g
