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

open Cmt_format

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for ML compiled objects. See graph_code.ml
 * and main_codegraph.ml for more information.
 * 
 * As opposed to lang_ml/analyze/graph_code_ml.ml, no need for:
 *  ???
 * 
 * schema:
 *  Root -> Dir -> Module -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  current: Graph_code.node;
  phase: phase;
}
 and phase = Defs | Uses

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let parse file =
  Common.memoized _hmemo file (fun () ->
    Cmt_format.read_cmt file
  )

let find_source_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  +> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | File_type.Obj "cmt" -> true
    | _ -> false
  ) +> Common.sort

let add_use_edge env dst =
  let src = env.current in
  match () with
  (* maybe nested function, in which case we dont have the def *)
  | _ when not (G.has_node src env.g) ->
    pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist (nested func?)"
           (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst env.g -> 
    G.add_edge (src, dst) G.Use env.g
  | _ -> 
    let (str, kind) = dst in
      (match kind with
      | _ ->
          let kind_original = kind in
          let dst = (str, kind_original) in

          G.add_node dst env.g;
          let parent_target = G.not_found in
          pr2 (spf "PB: lookup fail on %s (in %s)" 
                       (G.string_of_node dst) (G.string_of_node src));
          
          env.g +> G.add_edge (parent_target, dst) G.Has;
          env.g +> G.add_edge (src, dst) G.Use;
      )
(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let extract_defs_uses ~phase ~g ~ast ~readable =
  let env = {
    g; phase;
    current = (ast.cmt_modname, E.Module);
  }
  in
  if phase = Defs then begin
    let dir = Common.dirname readable in
    G.create_intermediate_directories_if_not_present g dir;
    g +> G.add_node env.current;
    g +> G.add_edge ((dir, E.Dir), env.current) G.Has;
  end;
  if phase = Uses then begin
    ast.cmt_imports +> List.iter (fun (s, digest) ->
      let node = (s, E.Module) in
      add_use_edge env node
    );
  end;
  
  ()


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir_or_file skip_list =
  let root = Common.realpath dir_or_file in
  let all_files = 
    find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let readable = Common.filename_without_leading_path root file in
      extract_defs_uses ~g ~ast ~phase:Defs ~readable;
      ()
    ));

  (* step2: creating the 'Use' edges *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let readable = Common.filename_without_leading_path root file in
      if readable =~ "^external" || readable =~ "^EXTERNAL"
      then ()
      else extract_defs_uses ~g ~ast ~phase:Uses ~readable;
      ()
    ));

  g
