
(* dependency structure matrix *)
type dm

(* list of nodes to expand *)
type config = Graph_code.node list

val build:
  config -> Graph_code.graph -> dm

(* poor's man DSM visualizer; use codegraph for a real visualization *)
val display:
  dm -> unit
