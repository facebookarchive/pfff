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
 * Module to create a Dependency Structure Matrix (DSM) based on
 * a code graph.
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 * See also main_codegraph.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Dependency Structure Matrix.
 * The relation between nodes is not stored here; you
 * can get such information in the code graph instead.
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

(* poor's man DSM visualizer (use codegraph for a real visualization) *)
let display dm =
  pr2_gen dm;
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build_with_nodes_order nodes g =
  let n = List.length nodes in

  let dm = {
    matrix = Common.make_matrix_init ~nrow:n ~ncolumn:n (fun i j -> 0);
    name_to_i = 
      Common.index_list_0 nodes +> Common.hash_of_list;
    i_to_name = 
      Common.index_list_0 nodes +> List.map Common.swap +> Common.hash_of_list;
  }
  in
  let hmemo = Hashtbl.create 101 in
  (* get the parent node of the node under consideration that is
   * displayed in the matrix.
   *)
  let rec projection n =
  (* todo: profile this? optimize? *)
    Common.memoized hmemo n (fun () ->
      if Hashtbl.mem dm.name_to_i n
      then Hashtbl.find dm.name_to_i n
      else
        (* can raise exn *)
        projection (G.parent n g)
    )
  in
  
  g +> G.iter_use_edges (fun n1 n2 ->
    (* todo? if expand do we create a line for the expanded? if no
     * then it will have no projection so this test below is not enough.
     * but may make sense to create a line for it which corresponds to
     * the difference with the children so for all edges that link
     * directly to this one?
     * 
     * note that if A depends on B, e.g. visual/ depends on commons/,
     * then we will increment 'row of visual' x 'column of commons',
     * that way one can easily see all the modules that visual/ depends
     * on by looking at the 'row of visual'.
     *)
    if n1 <> G.root then begin
      let i = projection n1 in
      let j = projection n2 in
      dm.matrix.(i).(j) <- dm.matrix.(i).(j) + 1;
    end
  );
  dm



let build config g =
  
  let top_nodes = G.succ G.root G.Has g in

  (* todo: iterate while in config *)
  let nodes = top_nodes in

  (* first draft *)
  let dm = build_with_nodes_order nodes g in

  (* Now we need to reorder to minimize the number of dependencies in the
   * top right corner of the matrix.
   * 
   * todo: optimize? we redo some computations ... should memoize
   * more? or just invert rows/columns in the original matrix?
   * 
   * note: sorting by the number of cells which you depend on is not
   * enough. For instance let's say X depends on 3 cells, A, B, Y and Y 
   * depends on 4: A, B, C, D. One could consider to put X
   * upper in the matrix, but because X depends on Y, it's better to put
   * Y upper.
   * 
   *)
  let nodes_with_scrore =
    nodes +> List.map (fun n ->
      let i = Hashtbl.find dm.name_to_i n in
      (* pr2_gen (n, i, dm.matrix.(i)); *)
      let cnt = ref 0 in
      
      dm.matrix.(i) +> Array.iteri (fun j e ->
        if e > 0 && j <> i  then begin 
          (* incr e ? *)
          incr cnt;
          end
      );
      n, !cnt
    ) +> Common.sort_by_val_lowfirst
  in
  pr2_gen nodes_with_scrore;
  build_with_nodes_order (nodes_with_scrore +> List.map fst) g

