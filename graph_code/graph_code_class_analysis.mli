
(* It can actually be a DAG, as with interfaces and traits one can fake
 * multiple inheritance.
 *)
val class_hierarchy: Graph_code.graph -> Graph_code.node Graph.graph

(* Return the toplevel methods for each method name in the graph.
 * The returned hashtbl use the Hashtbl.find_all property 
 *)
val toplevel_methods: 
  Graph_code.graph -> (string, Graph_code.node) Hashtbl.t
