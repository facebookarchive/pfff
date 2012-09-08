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

(* Dependency Structure Matrix *)
type dm = {
  matrix: int array array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: (int, Graph_code.node) Hashtbl.t;
  (* which nodes are currently expanded *)
  config: config;
}
  (* could reuse Common.tree2 *)
  and tree =
    | Node of Graph_code.node * tree list
  and config = tree

let basic_config g = 
  Node (G.root, G.succ G.root G.Has g +> List.map (fun n -> Node (n, [])))

type deps_style = 
  | DepsIn
  | DepsOut
  | DepsInOut

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* get the parent node of the node under consideration that is
 * displayed in the matrix.
 * opti: share a global hmemo?
 *)
let rec projection hmemo n dm g =
  (* todo: profile this? optimize? *)
  Common.memoized hmemo n (fun () ->
    match () with
    | _ when Hashtbl.mem dm.name_to_i n -> Some (Hashtbl.find dm.name_to_i n)
    (* it's possible we operate on a slice of the original dsm in which case
     * the projection of an edge can not project on anything
     * in the current name_to_i hash
     *)
    | _ when n = G.root -> None
    | _ -> projection hmemo (G.parent n g) dm g
  )

let rec final_nodes_of_tree tree =
  match tree with
  | Node (n, xs) ->
      if null xs 
      then [n]
      else List.map final_nodes_of_tree xs +> List.flatten

(*****************************************************************************)
(* Display *)
(*****************************************************************************)

(* poor's man DSM visualizer (use codegraph for a real visualization) *)
let display dm =
  pr2_gen dm;
  ()

(*****************************************************************************)
(* Building the matrix *)
(*****************************************************************************)

let build_with_tree tree g =

  (* todo? if expand do we create a line for the expanded? if no
   * then it will have no projection so the test below is not enough.
   * but may make sense to create a line for it which corresponds to
   * the difference with the children so for all edges that link
   * directly to this one?
   * 
   * note that if A depends on B, e.g. visual/ depends on commons/,
   * then we will increment 'row of visual' x 'column of commons',
   * that way one can easily see all the modules that visual/ depends
   * on by looking at the 'row of visual'.
   *)
  let nodes = final_nodes_of_tree tree in
  let n = List.length nodes in

  let dm = {
    matrix = Common.make_matrix_init ~nrow:n ~ncolumn:n (fun i j -> 0);
    name_to_i = 
      Common.index_list_0 nodes +> Common.hash_of_list;
    i_to_name = 
      Common.index_list_0 nodes +> List.map Common.swap +> Common.hash_of_list;
    config = tree;
  }
  in
  let hmemo = Hashtbl.create 101 in
  
  g +> G.iter_use_edges (fun n1 n2 ->
    if n1 <> G.root then begin
      let i = projection hmemo n1 dm g in
      let j = projection hmemo n2 dm g in
      (match i, j with
      | Some i, Some j ->
          dm.matrix.(i).(j) <- dm.matrix.(i).(j) + 1
      | _ -> ()
      )
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
  

(* opti: we redo many times the iteration on all edges ... for
 * different configuration. Can factorize work ?
 *)
let build tree g =

  (* let's compute a better reordered tree *)  
  let rec aux tree =
    match tree with
    | Node (n, xs) ->
        if null xs 
        then Node (n, [])
        else begin
          let children_nodes = 
            xs +> List.map (function (Node (n2, _)) -> n2) 
          in
          let h_children_of_children_nodes = 
            xs +> List.map (function (Node (n2, xs)) -> n2, xs) +> 
              Common.hash_of_list
          in
          
          let config_depth1 = 
            Node (n,xs +> List.map (function (Node (n2, _)) -> (Node (n2, []))))
          in
        
          (* first draft *)
          let dm = build_with_tree config_depth1 g in
          
          (* Now we need to reorder to minimize the number of dependencies in
           * the top right corner of the matrix.
           *)
          let nodes_reordered = partition_matrix children_nodes dm in
          Node (n,
               nodes_reordered +> List.map (fun n2 ->
                 let xs = Hashtbl.find h_children_of_children_nodes n2 in
                 (* recurse *)
                 aux (Node (n2, xs))
               )
          )
        end
  in
  
  let ordered_config = aux tree in
  build_with_tree ordered_config g

(*****************************************************************************)
(* Explain the matrix *)
(*****************************************************************************)
(* opti: there is probably a more efficient way to do that ... *)
let explain_cell_list_use_edges (i, j) dm g =
  let res = ref [] in
  let hmemo = Hashtbl.create 101 in

  g +> G.iter_use_edges (fun n1 n2 ->
    if n1 <> G.root then begin

      let i2 = projection hmemo n1 dm g in
      let j2 = projection hmemo n2 dm g in
      (match i2, j2 with
      | Some i2, Some j2 ->
          if i2 = i && j2 = j 
          then Common.push2 (n1, n2) res
      | _ -> ()
      )
    end
  );
  !res

(*****************************************************************************)
(* tree config manipulation *)
(*****************************************************************************)
let expand_node n tree g =
  let rec aux tree =
    match tree with
    | Node (n2, xs) ->
        if n = n2
        then 
          (* less: assert null xs? *)
          let succ = G.succ n G.Has g in
          Node (n2, succ +> List.map (fun n -> Node (n, [])))
        else Node (n2, xs +> List.map aux)
  in
  aux tree

let focus_on_node n deps_style tree dm =
  let deps = ref [] in
  let i = Hashtbl.find dm.name_to_i n in
  let nb_elts = Array.length dm.matrix in
  for j = 0 to nb_elts - 1 do
    let to_include =
      match deps_style with
      | DepsOut -> dm.matrix.(i).(j) > 0
      | DepsIn -> dm.matrix.(j).(i) > 0
      | DepsInOut -> dm.matrix.(i).(j) > 0 || dm.matrix.(j).(i) > 0
    in
   (* we do || i = j because we want the node under focus in too, in the 
    * right order
    *)
    if to_include || i = j
    then Common.push2 j deps
  done;
  Node (G.root, !deps +> List.rev +> List.map (fun i ->
    Node (Hashtbl.find dm.i_to_name i, []))
  )
  
