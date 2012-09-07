
(* dependency structure matrix *)
type dm = {
  matrix: int array array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: (int, Graph_code.node) Hashtbl.t;
}

(* list of nodes to expand *)
type config = Graph_code.node list

val build:
  config -> Graph_code.graph -> dm

val explain_cell_list_use_edges: 
  (int * int) -> dm -> Graph_code.graph ->
  (Graph_code.node * Graph_code.node) list

(* poor's man DSM visualizer (use codegraph for real visualization) *)
val display:
  dm -> unit
