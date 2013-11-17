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
 *  - better layout algorithm, minimize more backward dependencies
 *  - packing in "..." intermediate directories
 * 
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
  Node (G.root, Graph_code.children G.root g 
    +> List.map (fun n -> Node (n, [])))
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

type cell_coord = int * int

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

let hashtbl_find_node h n =
  try Hashtbl.find h n
  with Not_found ->
    (* pr2 (spf "PB: %s" (G.string_of_node n));*)
    (* raise Not_found *)
    failwith (spf "Not_found: %s" (G.string_of_node n))

let hashtbl_find h n =
  try Hashtbl.find h n
  with Not_found ->
    pr2_gen ("PB:", n);
    raise Not_found

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
  count_column (hashtbl_find_node dm.name_to_i n) m dm = 0

let count_row i m dm =
  let n = Array.length m in
  let cnt = ref 0 in
  for j = 0 to n - 1 do
    if m.(i).(j) > 0 && i <> j
    then (* incr cnt *) cnt := !cnt + formula (m.(i).(j))
  done;
  !cnt

let is_empty_row n m dm = 
  count_row (hashtbl_find_node dm.name_to_i n) m dm = 0

let empty_all_cells_relevant_to_node m dm n =
  let i = hashtbl_find_node dm.name_to_i n in
  let n = Array.length m in
  for x = 0 to n - 1 do
    m.(i).(x) <- 0;
    m.(x).(i) <- 0;
  done

let sort_by_count_rows_low_first xs m dm =
  xs +> List.map (fun n -> n, count_row (hashtbl_find_node dm.name_to_i n) m dm)
     +> Common.sort_by_val_lowfirst
     +> List.map fst

let sort_by_count_columns_high_first xs m dm =
  xs +> List.map (fun n -> n, count_column (hashtbl_find_node dm.name_to_i n) m dm)
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
    let idx = hashtbl_find_node dm.name_to_i n in
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
    (* pr2 (spf "step1: %s" (Common2.dump xs)); *)
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
    (* pr2 (spf "step2: %s" (Common2.dump xs)); *)
    left := !left ++ xs;
    if null xs
    then step1 rest
    else step2 rest
  in
  
  let rest = step2 nodes in
  if null rest
  then !left ++ !right
  else begin 
(*
    pr2 "CYCLE";
    pr2_gen rest;
*)
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
        let xs = hashtbl_find h s in
        let horder = xs +> Common.index_list_1 +> Common.hash_of_list in
        let current = ref 0 in
        let nodes_with_order = 
          nodes +> List.map (fun (s, node_kind) ->
            match Common2.hfind_option s horder with
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
(* Building a matrix *)
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
    igopti_to_idm.(hashtbl_find_node gopti.G2.name_to_i node) <- !i;
    incr i;
  );

  let dm = {
    matrix = Common2.make_matrix_init ~nrow:n ~ncolumn:n (fun i j -> 0);
    name_to_i = name_to_idm;
    i_to_name = idm_to_name;
    config = tree;
  }
  in
  let (projected_parent_of_igopti: idm idx array) = Array.create n_nodes (-1) in
  let (iroot: igopti idx) = hashtbl_find_node gopti.G2.name_to_i G.root in
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

(*****************************************************************************)
(* Create fake "a/b/..." directories *)
(*****************************************************************************)

(* design decisions, when should we pack?
 *  - in an adhoc manner in adjust_graph.txt
 *  - in the graph lazily while building the final config
 *  - in the graph lazily as a preprocessing phase on a full config
 *  - in an offline phase that packs everything? 
 *  - in the UI?
 * 
 * Packing lazily is good but it does not necessaraly work well with
 * the Focus because depending on our focus, we may have want different
 * packings. Also it makes it a bit hard to use cg from the command line
 * in a subdirectory. A solution could be to restart from a fresh gopti for
 * for each new focus.
 * 
 * Packing in the UI would be more flexible for the Focus, 
 * but we need lots of extra logic whereas just abusing the Has and
 * reorganize the graph makes things (at first) easier.
 * 
 * There are some issues also on how packing and layering impact each other.
 * We may not want to pack things that affect a lot the number of
 * backward references once packed.
 *)

let threshold_pack = ref 30


(* Optionaly put less "relevant" entries under an extra "..." fake dir.
 *  
 * We used to do this phase in build() at the same time we were building
 * the new config. But doing too many things at the same time was complicated
 * so better to separate a bit things in a preprocessing phase
 * where here we just adjust gopti.
 * 
 * Moreover we wanted the heuristics to order and to pack to use different
 * schemes. For packing we want to put under "..." entries considered
 * irrelevant when looked globally, that is not look only at the
 * column count in the submatrix but in the whole matrix.
 * For instance if a/b/c/d is used a lot from e/, but not used
 * that much internally inside a/b/c, we still don't want
 * to put it under a extra "..." because this directory is globally
 * very important.
 * 
 * Moreover when in Focused mode, the children of a node
 * are actually not the full set of children, and so we could pack
 * things under a "..." that are incomplete? Hmmm at the same time
 * it can be good to do some specialized packing based on a Focus.
 *)
let adjust_gopti_if_needed_lazily tree gopti =
  let gopti = ref gopti in

  let rec aux (tree: tree) (brothers: Graph_code.node list) =
    match tree with
    | Node (n, xs) ->
      if null xs
      then Node (n, [])
      else 
        (* less: use the full list of children of n? xs can be a subset
         * because in a focused generated config
         *)
        if List.length xs <= !threshold_pack
        then 
          Node (n, xs +> List.map (fun (Node (n1, xs1)) -> 
            let more_brothers = 
              xs +> Common.map_filter (fun (Node (n2, _)) ->
                if n1 <> n2 then Some n2 else None
              ) 
            in
            aux (Node (n1, xs1)) (brothers ++ more_brothers)
          ))
        else begin
          let children_nodes = xs +> List.map (fun (Node (n,_)) -> n) in
          let config = (Node (n,
             (xs +> List.map (fun (Node (n, _)) -> Node (n, []))) ++
             (brothers +> List.map (fun n -> Node (n, [])))))
          in
          let dm = build_with_tree config !gopti in

          let score = children_nodes +> List.map (fun n ->
            let idx = hashtbl_find_node dm.name_to_i n in
            let m = dm.matrix in
            n, count_column idx m dm 
               + count_row idx m dm
            (* + m.(idx).(idx) / 3 *)
          ) +> Common.sort_by_val_highfirst
            +> List.map fst
          in
          (* minus one because after the packing we will have 
           * threshold_pack - 1 + the new entry = threshold_pack
           * and so we will not loop again and again.
           *)
          let (ok, to_pack) = Common2.splitAt (!threshold_pack - 1) score in
          (* pr2 (spf "REPACKING: TO_PACK = %s, TO_KEEP = %s" 
                 (Common.dump to_pack) (Common.dump ok)); *)
          let new_gopti, dotdotdot_entry = 
            Graph_code_opti.adjust_graph_pack_some_children_under_dotdotdot 
              n to_pack !gopti in
          gopti := new_gopti;
          Node (n,
             (ok ++ [dotdotdot_entry]) +> List.map (fun n ->
               (* todo: grab the children of n in the original config? *)
               Node (n, [])
              )
          )
        end
  in
  let adjusted_tree = aux tree [] in
  !gopti, adjusted_tree


(*****************************************************************************)
(* Building the matrix *)
(*****************************************************************************)

(* The tree passed is a configuration one would like to explore. Note
 * that after a focus, the children of a node in this tree may not contain
 * all the original children of this node.
 *)
let build tree constraints_opt gopti =

  let gopti, tree = adjust_gopti_if_needed_lazily tree gopti in

  (* let's compute a better reordered tree *)  
  let rec aux tree =
    match tree with
    | Node (n, xs) ->
        if null xs 
        then Node (n, [])
        else begin
          let config_depth1 = 
            Node (n,xs +> List.map (function (Node (n2,_)) -> (Node (n2, []))))
          in
          (* first draft *)
          let dm = build_with_tree config_depth1 gopti in

          let children_nodes = 
            xs +> List.map (function (Node (n2, _)) -> n2) in
          
          (* Now we need to reorder to minimize the number of dependencies in
           * the top right corner of the matrix (of the submatrix actually)
           *)
          let nodes_reordered = 
            partition_matrix children_nodes dm in
          let nodes_reordered = 
            optional_manual_reordering n nodes_reordered constraints_opt in

          let h_children_of_children_nodes = 
            xs +> List.map (function (Node (n2, xs)) -> n2, xs) +> 
              Common.hash_of_list
          in
          
          Node (n,
               nodes_reordered +> List.map (fun n2 ->
                 let xs = 
                   try Hashtbl.find h_children_of_children_nodes n2 
                   (* probably one of the newly created "..." child *)
                   with Not_found -> []
                 in
                 (* recurse *)
                 aux (Node (n2, xs))
               ))
        end
  in
  let ordered_config = aux tree in
  build_with_tree ordered_config gopti, gopti

(*****************************************************************************)
(* Explain the matrix *)
(*****************************************************************************)

(* history: 
 * - iterate over all edges
 * - iterate only on the children of i
 * - use graph_opti instead of the memoized projection index
 *)
let explain_cell_list_use_edges (i, j) dm gopti =
  let res = ref [] in

  let n_nodes = G2.nb_nodes gopti in
  let igopti_to_idm = Array.create n_nodes (-1) in
  dm.i_to_name +> Array.iteri (fun idm node ->
    igopti_to_idm.(hashtbl_find_node gopti.G2.name_to_i node) <- idm;
  );
  let (projected_parent_of_igopti: idm idx array) = Array.create n_nodes (-1) in
  let (iroot: igopti idx) = hashtbl_find_node gopti.G2.name_to_i G.root in
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
  let (src: igopti idx) = hashtbl_find gopti.G2.name_to_i dm.i_to_name.(i) in
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
                     

(*   
let explain_cell_list_use_edges a b c =
  Common.profile_code "DM.explain_cell" (fun () -> 
    explain_cell_list_use_edges2 a b c)
*)

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



(* To focus on a node we need to know its dependencies to filter
 * the irrelevant one and so we need a dm passed as a parameter.
 * This function is mainly used in a Model.config_of_path
 * where we fold over an initial dm and given a path element
 * expand or focus to get a new dm and so on.
 *)
let focus_on_node n deps_style tree dm =
  let i = hashtbl_find_node dm.name_to_i n in
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
   *    Node (hashtbl_find_node dm.i_to_name i, []))
   *  )
   *)
  let rec aux tree = 
    match tree with
    | Node (n2, []) ->
        let j = hashtbl_find_node dm.name_to_i n2 in
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
  Common2.some (aux tree)


(*****************************************************************************)
(* Path manipulation *)
(*****************************************************************************)

(* less: could be put in dependencies_matrix_code.ml *)
let put_expand_just_before_last_focus_if_not_children n xs g =
  let rec aux xs =
    match xs with
    | [] -> [Expand n]
    | x::xs ->
        (match x with
        | Expand _ -> x::aux xs
        | Focus (n2,style) ->
            let children = Graph_code_opti.all_children n2 g in
            if not (List.mem n children)
            then (Expand n)::x::xs
            else x::aux xs
        )
  in
  aux xs

let fix_path path g =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x::xs ->
        (match x with
        | Focus _ -> 
            aux (acc ++ [x]) xs
        | Expand (n) ->
            aux (put_expand_just_before_last_focus_if_not_children n acc g) xs
        )
  in
  aux [] path

let config_of_path (path: config_path) gopti =
  let path = fix_path path gopti in
  let initial_config = basic_config_opti gopti in
  (* pr2_gen path; *)
  path +> List.fold_left (fun (config, gopti) e ->
    match e with
    | Expand node ->
        expand_node_opti node config gopti, gopti
    | Focus (node, kind) ->
        let dm, gopti = build config None gopti in
        focus_on_node node kind config dm, gopti
  ) (initial_config, gopti)
  

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
let distance_entity (i, j) arr =
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
      if mat.(i).(j) > 0 && i <> j && distance_entity (j, i) arr > 0
      then has_users_outside_parent := true
  done;
  not !has_users_outside_parent && 
  (* the elements at the root can't have dependencies outside parents *)
  List.length parents > 1
  
let score_upper_triangle dm exclude_nodes =
  let score = ref 0 in
  let exclude_idx = exclude_nodes +> List.map (fun n -> 
    hashtbl_find_node dm.name_to_i n) in

  for i = 0 to Array.length dm.matrix -1 do
    for j = i + 1 to Array.length dm.matrix -1 do
      if (List.mem i exclude_idx) || (List.mem j exclude_idx)
      then ()
      else score := !score + dm.matrix.(i).(j)
    done
  done;
  !score

let score_downer_triangle dm exclude_nodes =
  let score = ref 0 in
  let exclude_idx = exclude_nodes +> List.map (fun n -> 
    hashtbl_find_node dm.name_to_i n) in

  for i = 0 to Array.length dm.matrix -1 do
    for j = 0 to i - 1 do
      if (List.mem i exclude_idx) || (List.mem j exclude_idx)
      then ()
      else score := !score + dm.matrix.(i).(j)
    done
  done;
  !score

let score_upper_triangle_nodes dm =
  let score = Array.create (Array.length dm.matrix) 0 in
  for i = 0 to Array.length dm.matrix -1 do
    for j = i + 1 to Array.length dm.matrix -1 do
      let v = dm.matrix.(i).(j) in
      score.(i) <- score.(i) + v;
      score.(j) <- score.(j) + v;
    done
  done;
  score +> Array.mapi (fun i v -> (dm.i_to_name.(i), v)) +> Array.to_list

let score_upper_triangle_cells dm =
  let res = ref [] in
  for i = 0 to Array.length dm.matrix -1 do
    for j = i + 1 to Array.length dm.matrix -1 do
      Common.push2 ((i, j), dm.matrix.(i).(j)) res
    done
  done;
  !res
