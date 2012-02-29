(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_php

module Flag = Flag_analyze_php
module Ast = Ast_php

module V = Visitor_php
module T = Type_php
module N = Namespace_php

module Db = Database_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * The goal of this module is to generate a dot file where we can see
 * the dependencies between files or directories of a source code based
 * on the internal dependencies in this file, such as the caller/callee
 * dependencies, or the global or class definition or whatever.
 * 
 * history: I was doing that for Eurosys'06 but in an ugly way 
 * where I was generating the dot instead of generating a graph 
 * that then will generically call a function to generate a dot file.
 * 
 * todo: see also facebook/flib_dependencies/ and 
 * facebook/check_module/graph_module.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type key = string (* dirname or filename without the '/' *)
type node = string (* full dirname *)
type edge = Direct

type dependency_graph = (key, node, edge) Ograph_simple.ograph_mutable

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let dir_of_id id db = 
  let file = Database_php.readable_filename_of_id id db in
  let str = Filename.dirname file in
  if str = "." then "root" else str

let depth_topdirs = 1

let topdir_of_id id db = 
  let file = dir_of_id id db in
  let xs = Common.split "/" file in
  Common.take_safe depth_topdirs xs +> Common.join "/"
  

let nodify_str s = 
  Str.global_replace (Str.regexp "[/.-]") "__" s


let dir_to_dir_dependencies db =
  raise Todo
(*
  let (g: dependency_graph) = new Ograph_simple.ograph_mutable in

TODO: this is not under foundation/, move it to database_php.ml
  Database_php_build.iter_files_and_topids db "ANALYZING:" (fun id file ->

    let dependencies = 
      Database_php.callees_of_id id db
    in
    dependencies +> List.iter (fun callsite ->
      let id_target = Callgraph_php.id_of_callsite callsite in
      
      let dir_src  = topdir_of_id id db in
      let dir_dest = topdir_of_id id_target db in
      let node_src = nodify_str dir_src in
      let node_dest = nodify_str dir_dest in
  
      g#add_node_if_not_present node_src dir_src;
      g#add_node_if_not_present node_dest dir_dest;

      if node_src <> node_dest 
      then g#add_arc (node_src, node_dest) Direct;
    )
  );
  
  let tmpfile = "/tmp/graph_php.dot" in
  Ograph_simple.print_ograph_generic
    ~str_of_key:(fun k -> k)
    ~str_of_node:(fun k node -> node)
    tmpfile
    g;
  Ograph_extended.launch_gv_cmd tmpfile;
  ()
*)
