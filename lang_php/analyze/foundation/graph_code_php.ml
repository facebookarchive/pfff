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

module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Graph of dependencies for PHP. See graph_code.ml and main_codegraph.ml
 * for more information. Yet another code database for PHP ...
 * 
 * See also old/graph_php.ml, facebook/flib_dependencies/,
 * facebook/check_module/graph_module.ml
 * 
 * schema:
 *  Root -> Dir -> File (.php) -> # TODO more fine grained dependencies
 *       -> Dir -> SubDir -> Module? -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers graph_code *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse file = 
  try 
    let cst = Parse_php.parse_program file in
    let ast = Ast_php_simple_build.program cst in
    ast
  with exn ->
    pr2 (spf "PROBLEM with %s, exn = %s" file (Common.exn_to_s exn));
    []

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_php.find_php_files_of_dir_or_files [root] in

  (* step0?: filter noisy modules/files *)
  let files = all_files in

  let g = G.create () in
  g +> G.add_node G.root;

  (* step1: creating the nodes and 'Has' edges, the defs *)

  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let _ast = parse file in
     ()
   ));

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
   ));

  g


