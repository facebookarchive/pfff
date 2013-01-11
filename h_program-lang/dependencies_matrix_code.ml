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
module G2 = Graph_code_opti

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Module to create a Dependency Structure Matrix (DSM) based on
 * a code graph.
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 * See also main_codegraph.ml
 * 
 * history:
 *  - naive version
 *  - projection cache, memoize the projection of a node: given a deep node, 
 *    what is the node present in the matrix that "represents" this deep node
 *  - full matrix pre-computation optimisation
 *  - compute lazily deep rows using only a subset of the edges
 *  - graph code opti, because using arrays is far more efficient than
 *    hashtbl and/or memoized hashtbl
 *  - remove full matrix, not anymore needed
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Dependency Structure Matrix.
 *  
 * If A depends on B, e.g. visual/ depends on commons/,
 * then we will increment 'row of visual' x 'column of commons',
 * that way one can easily see all the modules that visual/ depends
 * on by looking at the 'row of visual'.
 * 
 * todo: I think Ndepend does the reverse. Also I think Ndepends uses
 * a symetric matrix model where one can get more information by looking
 * at both directions. In one direction one can know how many entities A is
 * using in B, and in the other by how many entities in A some stuff in B
 * are used. For instance one can see that A is using many different functions
 * in B, and then can see that actually all those functions are used by
 * only one thing in A, in which case it's a sign that maybe this function
 * should be moved in B. This is done I think only when there is a clean
 * layered archi. When there are cycles then NDepend uses another color
 * for the cell.
 * 
 * todo: coupling/cohesion metrics! the dsm can be helpful to visualize
 * this? see patterns? use more colors?
 *)
type dm = {
  matrix: int array array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: Graph_code.node array;
  (* which nodes are currently expanded *)
  config: config;
}
  (* It's actually more a 'tree set' than a 'tree list' below 
   * when we pass the config to build(). Indeed it's build() which
   * will order this set according to some partitionning algorithm
   * that tries to "layer" the code.
   * less: could reuse Common.tree2. 
   *)
  and tree =
    | Node of Graph_code.node * tree list
  and config = tree


let basic_config g = 
  Node (G.root, G.succ G.root G.Has g +> List.map (fun n -> Node (n, [])))
let basic_config_opti gopti = 
  Node (G.root, Graph_code_opti.children G.root gopti
   +> List.map (fun n -> Node (n, [])))

type config_path_elem = 
  | Expand of Graph_code.node
  | Focus of Graph_code.node * deps_style

  and deps_style = 
   | DepsIn
   | DepsOut
   | DepsInOut

type config_path = config_path_elem list

(* We sometimes want to manually order certain entries in the matrix,
 * especially when the code is a mess with cycles everywhere in
 * which case the default partitionning algorithm does not help.
 * The hashtbl maps string nodes to the ordered list of children
 * we want. We use a hash and not a tree because at some point
 * we may want to specify the order only for certain deeply
 * nested directories in which case we will do a find -name "info.txt"
 * to build all the partial constraints.
 *)
type partition_constraints = 
  (string, string list) Hashtbl.t

let tasks = ref 16

(* Phantom types for safer array access between the graph_opti, dm, and
 * the full matrix dm. Not really used, but could one day.
 *)
type 'a idx = int
type idm
type igopti
type ifull

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let verbose = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
(* Ordering the rows/columns, "layering" *)
(*****************************************************************************)

let formula x =
  assert(x > 0);
  (* 1 + (int_of_float (log10 (float_of_int x))) *)
  x

let count_column j m dm =
  let n = Array.length m in
  let cnt = ref 0 in
  for i = 0 to n - 1 do
    if m.(i).(j) > 0 && i <> j
    then (* incr cnt *) 
      cnt := !cnt + formula (m.(i).(j))
  done;
  !cnt

let is_empty_column n m dm =
  count_column (Hashtbl.find dm.name_to_i n) m dm = 0

let count_row i m dm =
  let n = Array.length m in
  let cnt = ref 0 in
  for j = 0 to n - 1 do
    if m.(i).(j) > 0 && i <> j
    then (* incr cnt *) cnt := !cnt + formula (m.(i).(j))
  done;
  !cnt

let is_empty_row n m dm = 
  count_row (Hashtbl.find dm.name_to_i n) m dm = 0

let empty_all_cells_relevant_to_node m dm n =
  let i = Hashtbl.find dm.name_to_i n in
  let n = Array.length m in
  for x = 0 to n - 1 do
    m.(i).(x) <- 0;
    m.(x).(i) <- 0;
  done

let sort_by_count_rows_low_first xs m dm =
  xs +> List.map (fun n -> n, count_row (Hashtbl.find dm.name_to_i n) m dm)
     +> Common.sort_by_val_lowfirst
     +> List.map fst

let sort_by_count_columns_high_first xs m dm =
  xs +> List.map (fun n -> n, count_column (Hashtbl.find dm.name_to_i n) m dm)
     +> Common.sort_by_val_highfirst
     +> List.map fst

(* todo: alternatives while discussing with matthieu
 * - find the first row, which should be the lines with the smallest
 *   sum (as it will be part of the big sum of the upper triangular),
 *   remove then this line, and iterate
 * - find the first row by doing the sum of (cell line / sum cell column)
 *   which is a bit equivalent to normalize, to do sum of percentage.
 *)
let sort_by_count_rows_low_columns_high_first xs m dm =
  xs +> List.map (fun n ->
    let idx = Hashtbl.find dm.name_to_i n in
    let h =
      float_of_int (count_row idx m dm) 
      /. 
        (1. +. float_of_int (count_column idx m dm))
    in
    n, h
  ) +> Common.sort_by_val_lowfirst
    +> List.map fst

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
 *)
let partition_matrix nodes dm =

  let m = dm.matrix +> Array.map Array.copy in
  let n = Array.length m in
  for i = 0 to n - 1 do
    m.(i).(i) <- 0
  done;

  let left = ref [] in
  let right = ref [] in

  let rec step1 nodes = 
    (* "1. Identify system elements (or tasks) that can be determined (or
     * executed) without input from the rest of the elements in the matrix.
     * Those elements can easily be identified by observing an empty column
     * in the DSM. Place those elements to the left of the DSM. Once an
     * element is rearranged, it is removed from the DSM (with all its
     * corresponding marks) and step 1 is repeated on the remaining
     * elements."
     *)
    let elts_with_empty_columns, rest = 
      nodes +> List.partition (fun node -> is_empty_column node m dm) in
    let xs = sort_by_count_rows_low_first elts_with_empty_columns m dm in
    xs +> List.iter (empty_all_cells_relevant_to_node m dm);
    right := xs ++ !right;
    pr2 (spf "step1: %s" (Common.dump xs));
    if null xs
    then rest
    else step1 rest

  and step2 nodes =
    (* "2.Identify system elements (or tasks) that deliver no
     * information to other elements in the matrix. Those elements can
     * easily be identified by observing an empty row in the DSM. Place
     * those elements to the right of the DSM. Once an element is
     * rearranged, it is removed from the DSM (with all its corresponding
     * marks) and step 2 is repeated on the remaining elements."
     *)
    let elts_with_empty_lines, rest = 
      nodes +> List.partition (fun node -> is_empty_row node m dm) in
    let xs = sort_by_count_columns_high_first elts_with_empty_lines m dm in
    xs+> List.iter (empty_all_cells_relevant_to_node m dm);
    pr2 (spf "step2: %s" (Common.dump xs));
    left := !left ++ xs;
    if null xs
    then step1 rest
    else step2 rest
  in
  
  let rest = step2 nodes in
  if null rest
  then !left ++ !right
  else begin 
    pr2 "CYCLE";
    pr2_gen rest;
    let rest = sort_by_count_rows_low_columns_high_first rest m dm in
    (* TODO merge and iterate *)
    !left ++ rest ++ !right
  end

(* to debug the heuristics *)
let info_orders dm =
  dm.matrix +> Array.mapi  (fun i _ ->
    let nrow = (count_row i dm.matrix dm) in
    let ncol = (count_column i dm.matrix dm) in
    let h = float_of_int nrow /. (1. +. float_of_int ncol) in
    h,
    (spf "%-20s: count lines = %d, count columns = %d, H = %.2f"
      (fst (dm.i_to_name.(i)))
      nrow
      ncol
      h)
  ) +> Array.to_list 
    +> Common.sort_by_key_lowfirst
    +> List.iter (fun (_, s) ->
       pr2 s
    )

(*****************************************************************************)
(* Manual ordering *)
(*****************************************************************************)

let optional_manual_reordering (s, node_kind) nodes constraints_opt =
  match constraints_opt with
  | None -> nodes
  | Some h ->
      if Hashtbl.mem h s
      then begin
        let xs = Hashtbl.find h s in
        let horder = xs +> Common.index_list_1 +> Common.hash_of_list in
        let current = ref 0 in
        let nodes_with_order = 
          nodes +> List.map (fun (s, node_kind) ->
            match Common.hfind_option s horder with
            | None ->
                pr2 (spf "INFO_TXT: could not find %s in constraint set" s);
                (s, node_kind), !current
            | Some n ->
                current := n;
                (s, node_kind), n
          )
        in
        Common.sort_by_val_lowfirst nodes_with_order +> List.map fst
      end
      else begin 
        pr2 (spf "didn't find entry in constraints for %s" s);
        nodes
      end

(*****************************************************************************)
(* Building the matrix *)
(*****************************************************************************)

let build_with_tree2 tree gopti =

  (* todo? when we expand do we create a line for the expanded? if no
   * then it will have no projection so the test below is not enough.
   * but may make sense to create a line for it which corresponds to
   * the difference with the children so for all edges that link
   * directly to this one?
   * 
   *)
  let nodes = final_nodes_of_tree tree in
  let n = List.length nodes in
  let n_nodes = G2.nb_nodes gopti in

  let name_to_idm = Hashtbl.create (n / 2) in
  let idm_to_name = Array.create n ("", E.Dir) in
  let igopti_to_idm = Array.create n_nodes (-1) in

  let (i: idm idx ref) = ref 0 in
  nodes +> List.iter (fun node ->
    Hashtbl.add name_to_idm node !i;
    idm_to_name.(!i) <- node;
    igopti_to_idm.(Hashtbl.find gopti.G2.name_to_i node) <- !i;
    incr i;
  );

  let dm = {
    matrix = Common.make_matrix_init ~nrow:n ~ncolumn:n (fun i j -> 0);
    name_to_i = name_to_idm;
    i_to_name = idm_to_name;
    config = tree;
  }
  in
  let (projected_parent_of_igopti: idm idx array) = Array.create n_nodes (-1) in
  let (iroot: igopti idx) = Hashtbl.find gopti.G2.name_to_i G.root in
  let rec depth parent igopti =
    let children = gopti.G2.has_children.(igopti) in
    let idm = igopti_to_idm.(igopti) in
    let project = 
      if idm = -1 
      then parent
      else idm
    in
    projected_parent_of_igopti.(igopti) <- project;
    children +> List.iter (depth project);
  in
  depth (-1) iroot;

  gopti.G2.use +> Array.iteri (fun i xs ->
    let parent_i = projected_parent_of_igopti.(i) in
    xs +> List.iter (fun j ->
      let parent_j = projected_parent_of_igopti.(j) in
      (* It's possible we operate on a slice of the original dsm, 
       * for instance when we focus on a node, in which case
       * the projection of an edge can not project on anything
       * in the current matrix.
       *)
      if parent_i <> -1 && parent_j <> -1
      then 
        dm.matrix.(parent_i).(parent_j) <- 
          dm.matrix.(parent_i).(parent_j) + 1
    )
  );
  dm

let build_with_tree a b = 
  Common.profile_code "DM.build_with_tree" (fun () -> build_with_tree2 a b)


let build tree constraints_opt gopti =

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
          let dm = build_with_tree config_depth1 gopti in
          
          (* Now we need to reorder to minimize the number of dependencies in
           * the top right corner of the matrix.
           *)
          let nodes_reordered = 
            partition_matrix children_nodes dm in
          let nodes_reordered = 
            optional_manual_reordering n nodes_reordered constraints_opt in
          Node (n,
               nodes_reordered +> List.map (fun n2 ->
                 let xs = Hashtbl.find h_children_of_children_nodes n2 in
                 (* recurse *)
                 aux (Node (n2, xs))
               ))
        end
  in
  let ordered_config = aux tree in
  build_with_tree ordered_config gopti

(*****************************************************************************)
(* Explain the matrix *)
(*****************************************************************************)

(* history: 
 * - iterate over all edges
 * - iterate only on the children of i
 * - use graph_opti instead of the memoized projection index
 *)
let explain_cell_list_use_edges2 (i, j) dm gopti =
  let res = ref [] in

  let n_nodes = G2.nb_nodes gopti in
  let igopti_to_idm = Array.create n_nodes (-1) in
  dm.i_to_name +> Array.iteri (fun idm node ->
    igopti_to_idm.(Hashtbl.find gopti.G2.name_to_i node) <- idm;
  );
  let (projected_parent_of_igopti: idm idx array) = Array.create n_nodes (-1) in
  let (iroot: igopti idx) = Hashtbl.find gopti.G2.name_to_i G.root in
  let rec depth parent igopti =
    let children = gopti.G2.has_children.(igopti) in
    let idm = igopti_to_idm.(igopti) in
    let project = 
      if idm = -1 
      then parent
      else idm
    in
    projected_parent_of_igopti.(igopti) <- project;
    children +> List.iter (depth project);
  in
  depth (-1) iroot;

  gopti.G2.use +> Array.iteri (fun i2 xs ->
    let parent_i2 = projected_parent_of_igopti.(i2) in
    xs +> List.iter (fun j2 ->
      let parent_j2 = projected_parent_of_igopti.(j2) in
      if parent_i2 = i && parent_j2 = j
      then 
       Common.push2 (
         gopti.G2.i_to_name.(i2), 
         gopti.G2.i_to_name.(j2)
       ) res;
    )
  );
(*
  let (src: igopti idx) = Hashtbl.find gopti.G2.name_to_i dm.i_to_name.(i) in
  let (dst: idm idx) = j in
  
  let rec aux n1 =
    let uses = gopti.G2.use.(n1) in
    uses +> List.iter (fun n2 ->
      let idm = igopti_to_idm.(n2) in
      if idm = dst
      then Common.push2 (gopti.G2.i_to_name.(n1), gopti.G2.i_to_name.(n2)) res;
    );
    let children = gopti.G2.has_children.(n1) in
    List.iter aux children
  in
  aux src;
*)
  !res
                     
   
let explain_cell_list_use_edges a b c =
  Common.profile_code "DM.explain_cell" (fun () -> 
    explain_cell_list_use_edges2 a b c)

(*****************************************************************************)
(* tree config manipulation *)
(*****************************************************************************)
let expand_node n tree g =
  let rec aux tree =
    match tree with
    | Node (n2, xs) ->
        if n =*= n2
        then 
          (* less: assert null xs? *)
          let succ = G.succ n G.Has g in
          Node (n2, succ +> List.map (fun n -> Node (n, [])))
        else Node (n2, xs +> List.map aux)
  in
  aux tree

let expand_node_opti n tree g =
  let rec aux tree =
    match tree with
    | Node (n2, xs) ->
        if n =*= n2
        then 
          (* less: assert null xs? *)
          let succ = Graph_code_opti.children n g in
          Node (n2, succ +> List.map (fun n -> Node (n, [])))
        else Node (n2, xs +> List.map aux)
  in
  aux tree



let focus_on_node n deps_style tree dm =
  let i = 
    try Hashtbl.find dm.name_to_i n 
    with Not_found ->
      pr2_gen (n);
      pr2_gen dm;
      raise Not_found
  in
  let (deps: int list ref) = ref [] in
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
  (* old: this was not keeping the hierarchy (which can be a feature)
   *  Node (G.root, !deps +> List.rev +> List.map (fun i ->
   *    Node (Hashtbl.find dm.i_to_name i, []))
   *  )
   *)
  let rec aux tree = 
    match tree with
    | Node (n2, []) ->
        let j = Hashtbl.find dm.name_to_i n2 in
        if i = j || List.mem j !deps
        then Some (Node (n2, []))
        else None
    | Node (n2, xs) ->
        let xs = xs +> Common.map_filter aux in
        if null xs 
        then None
        else Some (Node (n2, xs))
  in
  (* should be a Some cos at least we have 'n' in the tree *)
  Common.some (aux tree)
  

(*****************************************************************************)
(* Config path *)
(*****************************************************************************)

let string_of_config_path_elem = function
  | Expand n -> 
      spf "Expand(%s)" (G.string_of_node n)
  | Focus (n, style) -> 
      spf "Focus%s(%s)" 
        (match style with
        | DepsIn -> "<-"
        | DepsOut -> "->"
        | DepsInOut -> "<->"
        )
        (G.string_of_node n)

let string_of_config_path xs = 
  xs +> List.map string_of_config_path_elem +> Common.join "/"

(*****************************************************************************)
(* Matrix analysis *)
(*****************************************************************************)
let is_dead_column j dm =
  let mat = dm.matrix in
  let has_user = ref false in
  for i = 0 to Array.length mat - 1 do
      if mat.(i).(j) > 0 && i <> j then has_user := true
  done;
  not !has_user

let is_dead_line i dm =
  let mat = dm.matrix in
  let use_stuff = ref false in
  for j = 0 to Array.length mat - 1 do
    if mat.(i).(j) > 0 && i <> j then use_stuff := true
  done;
  not !use_stuff


let parents_of_indexes dm =
  let arr = Array.create (Array.length dm.matrix) [] in
  let i = ref 0 in
  let rec aux acc tree =
    match tree with
    (* a leaf *)
    | Node (n, []) ->
      arr.(!i) <- List.rev acc;
      incr i
    (* a node *)
    | Node (n, xs) ->
      xs +> List.iter (aux (n::acc))
  in
  aux [] dm.config;
  arr

(* ex: dist  a/b/c to a/b/d/e should be ? *)
let distance_entity i j arr =
  let xs = arr.(i) in
  let ys = arr.(j) in
  let rec aux xs ys =
    match xs, ys with
    | [], [] -> 0
    | _, [] -> 1
    (* if it's a subentity of a brother, then distance should still be 0 *)
    | [], _ -> 0

    | x::xs, y::ys ->
      if x =*= y
      then aux xs ys
      else 1
  in
  aux xs ys

(* less: more fine grained internal modules in package where can see what
 * is the scope of the module. So can see stuff really important in
 * a whole package because they are really used outside this package,
 * so depth of escape > X. ===> remember max depth of escape
 * 0 = same module, 1, brother, etc.
 *)
let is_internal_helper j dm =
  let mat = dm.matrix in
  let arr = parents_of_indexes dm in

  let has_users_outside_parent = ref false in
  let parents = arr.(j) in
  for i = 0 to Array.length mat - 1 do
      if mat.(i).(j) > 0 && i <> j && distance_entity j i arr > 0
      then has_users_outside_parent := true
  done;
  not !has_users_outside_parent && 
  (* the elements at the root can't have dependencies outside parents *)
  List.length parents > 1
  

