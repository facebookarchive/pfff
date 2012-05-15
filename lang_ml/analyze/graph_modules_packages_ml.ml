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

open Ast_ml
module V = Visitor_ml
module G = Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Dependency visualization for OCaml code, the obsolete code.
 * See main_codegraph.ml for discussions about alternatives and
 * graph_code_ml.ml for the new way to visualize dependencies.
 * 
 * todo: move in old/
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* filename in readable path *)
type ml_graph = Common.filename Graph.graph

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* assumes get a readable path *)
let project ~package_depth file =
  let xs = Common.split "/" file in

  (* todo? do something for n = 1? *)
  let xs' =
    match xs with
    (* todo: pad specific ... *)
    | "external"::_-> 
        Common.take_safe 2 xs
    (* todo: pfff specific ... *)
    | "facebook"::"external"::x::xs-> 
        ["external";x]
    (* <=> dirname *)
    | _ ->
        if package_depth = 0 then xs
        else Common.list_init xs
  in
  let s = Common.join "/" xs' in
  if s = "" then "."
  else s

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let dependencies ?(verbose=true) ~with_extern ~package_depth dir =
  let root = Common.realpath dir in
  let files = Lib_parsing_ml.find_ml_files_of_dir_or_files [root] in

  (* step0: adjust the set of files, to exclude noisy modules
   * or modules that would introduce false positive when do the 
   * modulename ->file lookup.
   *)
  let files = 
    files +> Common.exclude (fun file ->
      (* less: could also do a pfff_dependencies that just care about mli
       * like my make doti
       *)
      let (d,b,e) = Common.dbe_of_filename file in
      let xs = Common.split "/" d in
      let ml_file = Common.filename_of_dbe (d,b,"ml") in

      (* todo: pfff specific ... *)
      let is_test_in_external =
        List.mem "external" xs &&
          xs +> List.exists (fun s ->
           match s with
           | "examples" | "tests" |  "test" 
           (* ocamlgraph and ocamlgtk specific *)
           | "dgraph" | "editor" | "view_graph" | "applications"
               -> true
           | _ -> false
          )
      in
      let is_test =
        xs +> List.exists (fun s ->
          match s with
          | "examples" | "tests" |  "test" -> true
          | _ -> false
        )
      in

      (* pad specific *)
      let is_old = List.mem "old" xs in

      (* some files like in pfff/external/core/ do not have a .ml
       * so at least index the mli. otherwise skip the mli
       *)
      let is_mli_with_a_ml =
        e = "mli" && Sys.file_exists ml_file
      in
      is_test_in_external || is_mli_with_a_ml || is_old || is_test
    )
  in
  let g = G.create () in

  (* step1: creating the nodes *)
  let h_module_to_node = Hashtbl.create 101 in

  (* The PARSING_ERROR node beliw is in comment for now because 
   * it screws the graph. Moreover when in package mode, we don't want one 
   * of the file to make the whole package link to PARSING_ERROR. 
   * Moreover with a parsing error only the out-edges are missing;
   * the in-edges will work.
   * old: g +> G.add_vertex_if_not_present "PARSING_ERROR"; 
   *)

  files +> List.iter (fun file ->
    let readable = Common.filename_without_leading_path root file in
    let node = project ~package_depth readable in
    g +> G.add_vertex_if_not_present node;
    let m = Module_ml.module_name_of_filename readable in
    Hashtbl.add h_module_to_node m node;
  );
  (* the hierarchical support is not that good in gephi right now from
   * what I've seen
   * 
   * let _tree = 
   * files 
   * +> Treemap.tree_of_dirs_or_files ~file_hook:(fun f -> ())
   * +> Common.map_tree
   * ~fnode:(fun f -> Common.filename_without_leading_path root f)
   * ~fleaf:(fun (f, _) -> Common.filename_without_leading_path root f)
   * ()
   * in
   * let _ = tree +> Common.map_tree
   * ~fnode:(fun dir -> g +> G.add_vertex_if_not_present dir)
   * ~fleaf:(fun f -> f)
   * in
   *)

  (* step2: creating edges *)
  files +> Common.index_list_and_total +> List.iter (fun (file, i, total) ->
    if verbose then pr2 (spf "processing: %s (%d/%d)" file i total);
    let readable = Common.filename_without_leading_path root file in
    let node1 = project ~package_depth  readable in
    let ast = 
      Common.save_excursion Flag_parsing_ml.show_parsing_error false (fun ()->
        Parse_ml.parse_program file 
      )
    in
    (* when do module A = Foo, don't want to consider calls like A.foo *)
    let h_module_aliases = Hashtbl.create 101 in

    let add_edge_if_existing_module s =
      if Hashtbl.mem h_module_aliases s
      then () 
      else 
       if Hashtbl.mem h_module_to_node s
       then 
        (match Hashtbl.find_all h_module_to_node s with
        | [node2] ->
            (* todo? do weighted graph? but then if do some pattern matching
             * on 20 constructors, is it more important than
             * 2 functions calls? Need to differentiate those different
             * use of the qualifier
             *)
            if node1 <> node2
            then g +> G.add_edge node1 node2

        | _ -> ()
        )
      else
        if not with_extern 
        then pr2_once (spf "PB: could not find %s" s)
        else begin
          let node2 = "EXTERN/" ^ s in 
          g +> G.add_vertex_if_not_present node2;
          g +> G.add_edge node1 node2;
        end
    in

    let visitor = V.mk_visitor { V.default_visitor with
      V.ktoplevel = (fun (k, _) x ->
        match x with
        | NotParsedCorrectly _ ->
            (* g +> G.add_edge node1 "PARSING_ERROR"; *)
            ()
        | _ -> k x
      );
      (* todo? does it cover all use cases of modules ? maybe need
       * to introduce a kmodule_name_ref helper in the visitor
       * that does that for us.
       * todo: if want to give more information on edges, need
       * to intercept the module name reference at a upper level
       * like in FunCallSimple. C-s for long_name in ast_ml.ml
       *)
      V.kmodule_expr = (fun (k, _) x ->
        (match x with
        | ModuleName (qu, (Name (s,_))) ->
            add_edge_if_existing_module s
        | _ -> ()
        );
        k x
      );
      V.kitem = (fun (k, _) x ->
        (match x with
        | Open (_tok, (qu, (Name (s,_)))) ->
            add_edge_if_existing_module s
        | Module (_, Name (s,_), _, (ModuleName _)) ->
            Hashtbl.add h_module_aliases s true;
        | _ -> ()
        );
        k x
      );
      V.kqualifier = (fun (k, _) xs ->
        (match xs with 
        | [Name (s, _), _tok] ->
            add_edge_if_existing_module s
        | _ -> ()
        );
        k xs
      );
    }
    in
    visitor (Program ast);
  );
  (* could put that in gephi.ml *)
  g +> G.add_vertex_if_not_present "SINGLE"; 
  g +> G.add_vertex_if_not_present "ONLY_TO_COMMON"; 
  let nodes = G.nodes g in
  nodes +> List.iter (fun n ->
    let succ = G.succ n g in
    let pred = G.pred n g in
    match succ, pred with
    | [], [] ->
        g +> G.add_edge n "SINGLE"
    | [x], [] ->
        if x = "commons"
        then g +> G.add_edge n "ONLY_TO_COMMON"

    | [], _ ->
        ()
    | _, [] ->
        ()
    | x::xs, y::ys ->
        ()
  );
  g
