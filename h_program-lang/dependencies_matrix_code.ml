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


(* 
 * See http://dsmweb.org/dsmweb/en/understand-dsm/technical-dsm-tutorial/partitioning.html
 * 
 * note: sorting by the number of cells which you depend on is not
 * enough. For instance let's say X depends on 3 cells, A, B, Y and Y 
 * depends on 4: A, B, C, D. One could consider to put X
 * upper in the matrix, but because X depends on Y, it's better to put
 * Y upper.
 * 
 * todo: optimize? we redo some computations ... should memoize
 * more? or just invert rows/columns in the original matrix?
 * 

 *)
let partition_matrix nodes dm =

  let m = dm.matrix +> Array.map Array.copy in
  let n = Array.length m in
  for i = 0 to n - 1 do
    m.(i).(i) <- 0
  done;

  (* "1. Identify system elements (or tasks) that can be determined (or
   * executed) without input from the rest of the elements in the matrix.
   * Those elements can easily be identified by observing an empty column
   * in the DSM. Place those elements to the left of the DSM. Once an
   * element is rearranged, it is removed from the DSM (with all its
   * corresponding marks) and step 1 is repeated on the remaining
   * elements."
   *)

  (* "2.Identify system elements (or tasks) that deliver no
   * information to other elements in the matrix. Those elements can
   * easily be identified by observing an empty row in the DSM. Place
   * those elements to the right of the DSM. Once an element is
   * rearranged, it is removed from the DSM (with all its corresponding
   * marks) and step 2 is repeated on the remaining elements."
   *)
  let rec step2 todo_nodes =
    match todo_nodes with
    | [] -> []
    | x::xs ->
        let elts_with_empty_row, rest =
          todo_nodes +> List.partition (fun node ->
            let i = Hashtbl.find dm.name_to_i node in
            let cnt = ref 0 in
            for j = 0 to n - 1 do
              if m.(i).(j) > 0 then incr cnt
            done;
            !cnt = 0
          )
        in
        pr2_gen elts_with_empty_row;
        (match elts_with_empty_row with
        (* cycle? not a layered system ... stop for now *)
        | [] -> 
            pr2 "CYCLE";
            pr2_gen todo_nodes;
            todo_nodes
        | x::xs ->
            elts_with_empty_row +> List.iter (fun node ->
              let j = Hashtbl.find dm.name_to_i node in
              (* set to 0 the corresponding cells in the matrix *)
              for i = 0 to n - 1 do
                m.(i).(j) <- 0
              done;
            );
            elts_with_empty_row ++ step2 rest
        )
  in
  let done_nodes_step1 = step2 nodes in
  done_nodes_step1
  


let build config g =
  
  let top_nodes = G.succ G.root G.Has g in

  (* iterate while in config *)
  let nodes = 
    let rec aux xs =
      match xs with
      | [] -> []
      | x::xs ->
          if List.mem x config
          then
            let children = G.succ x G.Has g in
            aux children ++ aux xs
          else x :: aux xs
    in
    aux top_nodes
  in

  (* first draft *)
  let dm = build_with_nodes_order nodes g in

  (* Now we need to reorder to minimize the number of dependencies in the
   * top right corner of the matrix.
   *)
  let nodes_reordered = partition_matrix nodes dm in
  build_with_nodes_order nodes_reordered g

