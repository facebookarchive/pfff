
(* filename in readable path *)
type ml_graph = Common.filename Graph.graph

val dependencies: 
  ?verbose:bool ->
  with_extern:bool ->
  package_depth: int ->
  Common.dirname -> ml_graph
