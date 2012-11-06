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
 *  - nested classes are compiled in another class with a $
 *  - generics?
 * 
 * todo: put back nested classes inside the other?
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

let parse ~show_parse_error file =
  try 
    Parse_bytecode.parse file
  with exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file 
                  (Common.exn_to_s exn));
    raise exn


let package_and_name_of_str name =
  let xs = Common.split "\\." name in
  let package = List.rev (List.tl (List.rev xs)) in
  (package, name)

let package_and_name_of_cname class_name =
  let name = JBasics.cn_name class_name in
  package_and_name_of_str name



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

let add_use_edge g (src, dst) =
  match () with
  | _ when not (G.has_node src g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
              (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst g -> 
      G.add_edge (src, dst) G.Use g

  | _ -> 
    let (name, kind) = dst in
    let fake_name = 
      (Common.split "\\." name) 
      +> List.map (fun s -> s^"2") 
      +> Common.join "."
    in
    let dst = (fake_name, kind) in
    let parent_target = G.not_found in
    if not (G.has_node dst g)
    then begin 
      let (fake_package, _name) = package_and_name_of_str fake_name in
      let parent = create_intermediate_packages_if_not_present 
        g parent_target fake_package in
      pr2 (spf "PB: lookup fail on %s (in %s)" 
             (G.string_of_node dst) (G.string_of_node src));
      g +> G.add_node dst;
      g +> G.add_edge (parent, dst) G.Has;
    end;
    g +> G.add_edge (src, dst) G.Use;
    ()
              

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
let extract_defs ~g ast =
  let jclass = ast in

  let (package, name) = package_and_name_of_cname jclass.j_name in
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
  let jclass = ast in
  let name = JBasics.cn_name jclass.j_name in
  let current = (name, E.Class E.RegularClass) in

  let parents = Common.option_to_list jclass.j_super ++ jclass.j_interfaces in

  parents +> List.iter (fun cname ->
    let node = (JBasics.cn_name cname, E.Class E.RegularClass) in
    
    add_use_edge g (current, node);
  );
  ()

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
      let ast = parse ~show_parse_error:true file in
      extract_defs ~g ast;
      ()
    ));

  (* step2: creating the 'Use' edges *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let ast = parse ~show_parse_error:false  file in
     let readable = Common.filename_without_leading_path root file in
     if readable =~ "^external" || readable =~ "^EXTERNAL"
     then ()
     else 
       extract_uses ~g ast;
     ()
   ));

  g

