
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

(* poor's man DSM visualizer; use codegraph for a real visualization *)
val display:
  dm -> unit
