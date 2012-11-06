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

open JClassLow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for bytecode. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * As opposed to lang_java/analyze/graph_code_java.ml, no need for:
 *  - package/class lookup (all names are resolved already)
 *  - ???
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  phase: phase;
}
  and phase = Defs | Uses

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse file =
  Parse_bytecode.parse file

(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =

  (* ["java";"lang"] -> [[]; ["java"]; ["java";"lang"] ] *)
  let dirs = Common.inits xs in
  let dirs = 
    match dirs with
    | []::xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> current
    | x::xs ->
      let str = Common.join "." x in
      let entity = str, E.Package in
      if G.has_node entity g
      then aux entity xs
      else begin
        g +> G.add_node entity;
        g +> G.add_edge (current, entity) G.Has;
        aux entity xs
      end
  in
  aux root dirs

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
let extract_defs ~g ast =
  let jclass = ast in
  let name = JBasics.cn_name jclass.j_name in

  let xs = Common.split "\\." name in
  let package = List.rev (List.tl (List.rev xs)) in
  
  let current = create_intermediate_packages_if_not_present g G.root package in

  let node = (name, E.Class E.RegularClass) in
  g +> G.add_node node;
  g +> G.add_edge (current, node) G.Has;

  let current = node in

  jclass.j_fields +> List.iter (fun fld ->
    let node = (name ^ "." ^ fld.f_name, E.Field) in
    g +> G.add_node node;
    g +> G.add_edge (current, node) G.Has;
  );
  jclass.j_methods +> List.iter (fun def ->
    let node = (name ^ "." ^ def.m_name, E.Method E.RegularMethod) in

    (* less: for now we just collapse all methods with same name together *)
    if G.has_node node g
    then ()
    else begin
      g +> G.add_node node;
      g +> G.add_edge (current, node) G.Has;
    end
  );

  ()


(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

let extract_uses ~g ast =
  raise Todo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir_or_file skip_list =
  let root = Common.realpath dir_or_file in
  let all_files = 
    Lib_parsing_bytecode.find_source_files_of_dir_or_files [root] in

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
      extract_defs ~g ast;
      ()
    ));

  (* step2: creating the 'Use' edges *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let _readable = Common.filename_without_leading_path root file in
     let _ast = parse file in
     ()
   ));

  g

