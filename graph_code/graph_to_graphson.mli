open Graph_code

(*Prints GraphSON object to file "graphson.json"*)
val graphson_print: Graph_code.graph -> unit

(*Given Graph_code.graphs, parse to Graph_graphson_piqi.graph_graphson*)
val graph_graphson_piqi:graph -> Graph_graphson_piqi.graph_graphson

(* Parses instance of nodeinfo Hashtbl.t from type graph, and makes a note of the
 * node. Multiple definitions of node isn't allowed 
 *)
val add_graphson_vertex: node -> Graph_code.nodeinfo ->
    unit
    
(* Parses instance of edgeinfo Hashtbl2.t from type graph, makes note of edges.
 * Multiple definitions is not allowed
 *)
val add_graphson_edge: (Graph_code.node * Graph_code.node * Graph_code.edge ) ->
    Graph_code.edgeinfo -> unit

