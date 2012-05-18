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

(* Dependency Structure Matrix.
 * The relation between nodes is not stored here. But you
 * can get such information in the code graph.
 *)
type dm = {
  matrix: int array array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: (int, Graph_code.node) Hashtbl.t;
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
    name_to_i = 
      Common.index_list_0 nodes +> Common.hash_of_list;
    i_to_name = 
      Common.index_list_0 nodes +> List.map Common.swap +> Common.hash_of_list;
  }
  in
  (* todo: profile this? optimize? *)
  let hmemo = Hashtbl.create 101 in
  let rec projection n =
    Common.memoized hmemo n (fun () ->
      if Hashtbl.mem dm.name_to_i n
      then Hashtbl.find dm.name_to_i n
      else
        projection (G.parent n g)
    )
  in
  
  g +> G.iter_use_edges (fun n1 n2 ->
    (* todo? if expand do we create a line for the expanded? if no
     * then it will have no projection so this test below is not enough.
     * but may make sense to create a line for it which corresponds to
     * the difference with the children so for all edges that link
     * directly to this one?
     *)
    if n1 <> G.root then begin
      let i = projection n1 in
      let j = projection n2 in
      dm.matrix.(i).(j) <- dm.matrix.(i).(j) + 1;
    end
  );
  dm
