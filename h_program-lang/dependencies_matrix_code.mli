
(* dependency structure matrix *)
type dm = {
  matrix: int array array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: (int, Graph_code.node) Hashtbl.t;
  config: config;
}
  and tree =
    | Node of Graph_code.node * tree list
  and config = tree

(* just the expanded root *)
val basic_config: Graph_code.graph -> config

val build:
  config -> Graph_code.graph -> dm

val explain_cell_list_use_edges: 
  (int * int) -> dm -> Graph_code.graph ->
  (Graph_code.node * Graph_code.node) list

(* tree config manipulation *)
val expand_node: Graph_code.node -> tree -> Graph_code.graph -> tree

(* poor's man DSM visualizer (use codegraph for real visualization) *)
val display:
  dm -> unit
