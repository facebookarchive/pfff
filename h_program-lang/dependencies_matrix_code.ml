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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Module to create a Depdency Structure Matrix (DSM) based on
 * a code graph.
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 * See also main_codegraph.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* dependency structure matrix *)
type dm = {
  matrix: int array array;
  (* could be a tree *)
  names: Graph_code.node array;
}

(* list of nodes to expand *)
type config = Graph_code.node list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Display *)
(*****************************************************************************)

(* poor's man DSM visualizer; use codegraph for a real visualization *)
let display dm =
  pr2_gen dm;
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build config g =
  
  let top_nodes = G.succ G.root G.Has g in

  (* todo: iterate while in config *)
  let nodes = top_nodes in
  let n = List.length nodes in

  let dm = {
    matrix = Common.make_matrix_init ~nrow:n ~ncolumn:n (fun i j -> 0);
    names = Array.of_list nodes;
  }
  in
  dm

