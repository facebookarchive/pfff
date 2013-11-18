
(* it can actually be a DAG, as with interfaces and traits one can fake
 * multiple inheritance.
 *)
val class_hierarchy: Graph_code.graph -> Graph_code.node Graph.graph
