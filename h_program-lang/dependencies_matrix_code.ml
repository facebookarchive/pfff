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

(* optimization, given a deep node, what is the node present in the
 * matrix that "represents" this deep node.
 *)
type projection_cache = (Graph_code.node, Graph_code.node option) Hashtbl.t

let tasks = ref 4

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let verbose = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* get the parent node of the node under consideration that is
 * displayed in the matrix. The 'hmemo' passed should be related
 * to the 'dm' because two different matrices should lead to different
 * projections.
 * todo: profile this? optimize?
 *)
let rec projection2 hmemo n dm g =
  Common.memoized hmemo n (fun () ->
    match () with
    | _ when Hashtbl.mem dm.name_to_i n -> Some n
    (* It's possible we operate on a slice of the original dsm, for instance
     * when we focus on a node, in which case
     * the projection of an edge can not project on anything
     * in the current name_to_i hash.
     *)
    | _ when n = G.root -> None
    | _ -> projection2 hmemo (G.parent n g) dm g
  )
let projection a b c d =
  Common.profile_code "DM.projection" (fun () -> projection2 a b c d)

let projection_index a b dm d = 
  projection a b dm d +> Common.fmap (fun n -> Hashtbl.find dm.name_to_i n)

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
  1 + (int_of_float (log10 (float_of_int x)))

let count_column n m dm =
  let j = Hashtbl.find dm.name_to_i n in
  let n = Array.length m in
  let cnt = ref 0 in
  for i = 0 to n - 1 do
    if m.(i).(j) > 0 
    then (* incr cnt *) 
      cnt := !cnt + formula (m.(i).(j))
  done;
  !cnt

let is_empty_column n m dm =
  count_column n m dm = 0

let count_row n m dm =
  let i = Hashtbl.find dm.name_to_i n in
  let n = Array.length m in
  let cnt = ref 0 in
  for j = 0 to n - 1 do
    if m.(i).(j) > 0 
    then (* incr cnt *) cnt := !cnt + m.(i).(j)
  done;
  !cnt

let is_empty_row n m dm = 
  count_row n m dm = 0

let empty_all_cells_relevant_to_node m dm n =
  let i = Hashtbl.find dm.name_to_i n in
  let n = Array.length m in
  for x = 0 to n - 1 do
    m.(i).(x) <- 0;
    m.(x).(i) <- 0;
  done

let sort_by_count_rows_low_first xs m dm =
  xs +> List.map (fun n -> n, count_row n m dm)
     +> Common.sort_by_val_lowfirst
     +> List.map fst

let sort_by_count_columns_high_first xs m dm =
  xs +> List.map (fun n -> n, count_column n m dm)
     +> Common.sort_by_val_highfirst
     +> List.map fst

(* todo: do mix *)
let sort_by_count_rows_low_columns_high_first xs m dm =
  sort_by_count_columns_high_first xs m dm

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
      else nodes

(*****************************************************************************)
(* Building the matrix *)
(*****************************************************************************)

let build_with_tree2 tree hmemo full_matrix_opt g =

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
  (match full_matrix_opt with
  | None ->
      g +> G.iter_use_edges (fun n1 n2 ->
        if n1 <> G.root then begin
          let i = projection_index hmemo n1 dm g in
          let j = projection_index hmemo n2 dm g in
          (match i, j with
          | Some i, Some j ->
              dm.matrix.(i).(j) <- dm.matrix.(i).(j) + 1
          | _ -> ()
          )
        end
      );
  | Some fulldm ->
      let hdone = Hashtbl.create 101 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          let n1 = Hashtbl.find dm.i_to_name i in
          let n2 = Hashtbl.find dm.i_to_name j in

          try 
            let i' = Hashtbl.find fulldm.name_to_i n1 in
            let j' = Hashtbl.find fulldm.name_to_i n2 in
            dm.matrix.(i).(j) <- fulldm.matrix.(i').(j')
          (* the full matrix contains only the top k nodes,
           * so the name_to_i may fail ablove. In that case we
           * need to compute and go through all the edges under
           * and reproject.
           *)
          with Not_found ->
            if Hashtbl.mem hdone (i, j) then ()
            else begin
              (* let's compute the whole line for n1 *)
              if not (Hashtbl.mem fulldm.name_to_i n1) then begin
                (* similar to the opti we do in explain_cell *)
                pr2 ("computing row without full matrix help");
                let children = G.all_children n1 g in
                children +> List.iter (fun n1 ->
                  let uses = G.succ n1 G.Use g in
                  uses +> List.iter (fun n2 ->
                    let j2 = projection_index hmemo n2 dm g in
                    match j2 with
                    | Some j2 ->
                        if not (Hashtbl.mem hdone (i, j2)) then 
                          dm.matrix.(i).(j2) <- dm.matrix.(i).(j2) + 1;
                    | None -> 
                        (* if in Focus mode, the edge might not project
                         * on anything.
                         *)
                        ()
                  )
                );
                for j = 0 to n-1 do
                  Hashtbl.add hdone (i, j) true;
                done
              end;
              (* let's compute the whole row for n2 *)
              if not (Hashtbl.mem fulldm.name_to_i n2) then begin
                (* pr2 ("computing column without full matrix help");*)
              end;
            end
        done
      done
  );
  dm

let build_with_tree a b c d = 
  Common.profile_code "DM.build_with_tree" (fun () -> build_with_tree2 a b c d)


let build tree constraints_opt full_matrix_opt g =

  (* we call build_with_tree two times, with different order of nodes,
   * which does some redundant computation, but the projection at
   * least can be factorized by using the same projection_cache
   *)
  let (hmemo: projection_cache) = Hashtbl.create 101 in

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
          let dm = build_with_tree config_depth1 hmemo full_matrix_opt g in
          
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
               )
          )
        end
  in
  
  let ordered_config = aux tree in
  build_with_tree ordered_config hmemo full_matrix_opt g

(*****************************************************************************)
(* Building optimized matrix *)
(*****************************************************************************)

let threshold_nodes_full_matrix = 20000

(* todo: intelligent split of the tree? to avoid outliers? 
 * have a quota per subtree?
 *)
let top_nodes_of_graph_until_threshold g =

  let res = ref [] in
  let remaining = ref threshold_nodes_full_matrix in
  (* bfs-like algorithm *)
  let rec aux xs =
    if null xs 
    then ()
    else 
      if List.length xs > !remaining
      then ()
      else begin
        remaining := !remaining - (List.length xs);
        xs +> List.iter (fun x -> Common.push2 x res);
        aux (xs +> List.map (fun n -> G.succ n G.Has g) +> List.flatten)
      end
  in
  aux [G.root];
  (* old: g +> G.iter_nodes (fun n -> Common.push2 n res); *)
  !res

(* return the list of parents indexes (not nodes) *)
let rec parents2 hmemo n g dm =
  Common.memoized hmemo n (fun () ->
    if n = G.root 
    then []
    else
      let parent = G.parent n g in
      let i = Hashtbl.find dm.name_to_i n in
      i::parents2 hmemo parent g dm
  )

let parents a b c d =
  Common.profile_code "DM.parents" (fun () -> parents2 a b c d)

let update_matrix2 xs ys dm =
  xs +> List.iter (fun i ->
    ys +> List.iter (fun j ->
      dm.matrix.(i).(j) <- dm.matrix.(i).(j) + 1;
    )
  )

let update_matrix a b c =
  Common.profile_code "DM.update_matrix" (fun () -> update_matrix2 a b c)

let build_full_matrix2 g =
  let nodes = top_nodes_of_graph_until_threshold g in

  let n = List.length nodes in
  let n_all = G.nb_nodes g in
  pr2 (spf "Building full matrix, n = %d (%d)" n n_all);

  let name_to_i = Hashtbl.create (n / 2) in
  let i_to_name = Hashtbl.create (n / 2) in
  let i = ref 0 in
  pr2 (spf "Building nodes hashes");
  nodes +> List.iter (fun node ->
    Hashtbl.add name_to_i node !i;
    Hashtbl.add i_to_name !i node;
    incr i;
  );
  let dm = {
    matrix = Common.make_matrix_init ~nrow:n ~ncolumn:n (fun i j -> 0);
    name_to_i;
    i_to_name;
    config = Node (G.root, []);
  }
  in
  let hmemo_parents = Hashtbl.create (n / 2) in
  let hmemo_proj = Hashtbl.create (n / 2) in

  let n = G.nb_use_edges g in
  pr2 (spf "iterating %d edges" n);
(*
  Common_extra.execute_and_show_progress2 ~show:!verbose n (fun k ->
   g +> G.iter_use_edges (fun n1 n2 ->
    k();
*)
  let xs = G.all_use_edges g in
  let jobs = 
    xs +> List.map (fun (n1, n2) -> 
    fun () ->
    let n1 = projection_index hmemo_proj n1 dm g in
    let n2 = projection_index hmemo_proj n2 dm g in
    match n1, n2 with
    | Some n1, Some n2 ->
        let n1 = Hashtbl.find dm.i_to_name n1 in
        let n2 = Hashtbl.find dm.i_to_name n2 in
        let parents_n1 = parents hmemo_parents n1 g dm in
        let parents_n2 = parents hmemo_parents n2 g dm in
        (* cross product *)
        [(parents_n1, parents_n2)]
        (* update_matrix parents_n1 parents_n2 dm; *)
    | _ -> []
    
  )
  in
  let res = Parallel.map_batch_jobs ~tasks:!tasks jobs +> List.flatten in
  res +> List.iter (fun (parents_n1, parents_n2) ->
    update_matrix parents_n1 parents_n2 dm
  );
  dm

let build_full_matrix a = 
  Common.profile_code "DM.build_full_matrix" (fun () -> build_full_matrix2 a)

(*****************************************************************************)
(* Explain the matrix *)
(*****************************************************************************)

let explain_cell_list_use_edges2 hmemo (i, j) dm g =
  let res = ref [] in

  (* old: g +> G.iter_use_edges (fun n1 n2 -> *)
  let src = Hashtbl.find dm.i_to_name i in
  let children = G.all_children src g in
  children +> List.iter (fun n1 ->
    let uses = G.succ n1 G.Use g in
    uses +> List.iter (fun n2 ->

      if n1 <> G.root then begin
        let i2 = projection_index hmemo n1 dm g in
        let j2 = projection_index hmemo n2 dm g in
        (match i2, j2 with
        | Some i2, Some j2 ->
            if i2 = i && j2 = j 
            then Common.push2 (n1, n2) res
        | _ -> ()
        )
      end
    );
  );
  !res

let explain_cell_list_use_edges a b c d =
  Common.profile_code "DM.explain_cell" (fun () -> 
    explain_cell_list_use_edges2 a b c d)

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
