(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Objective C. See graph_code.ml and
 * main_codegraph.ml for more information.
 * 
 * 
 * schema:
 *  Root -> Dir -> File (.m|.h) 
 *       -> Dir -> SubDir -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_uses visitor *)
type env = {
  current: Graph_code.node;
  g: Graph_code.graph;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse ~show_parse_error file =
  try 
    Common.save_excursion Flag.verbose_lexing false (fun () ->
      Parse_cpp.tokens file
    )
  with 
  | Timeout -> raise Timeout
  | exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    raise exn

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

let extract_defs ~g ~ast ~readable =
  let dir = Common.dirname readable in
  G.create_intermediate_directories_if_not_present g dir;
  g +> G.add_node (readable, E.File);
  g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  ()

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_cpp.find_cpp_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse ~show_parse_error:true file in
      extract_defs ~g ~ast ~readable;
    ));

  g
