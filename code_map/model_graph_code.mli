(*s: model_graph_code.mli *)
val build_filedeps_of_dir_or_file: 
  Graph_code.graph ->
  (Graph_code.node, Common.filename list * Common.filename list) Hashtbl.t

(* the nodes are sorted by line numbers *)
val build_entities_of_file:
  Graph_code.graph ->
  (Common.filename, Graph_code.node list) Common.assoc

val add_headers_files_entities_of_file:
  Common.dirname -> 
  (Common.filename, Graph_code.node list) Common.assoc ->
  (Common.filename, Graph_code.node list) Common.assoc

val node_of_entity: 
  Database_code.entity -> Graph_code.graph -> Graph_code.node option
(*e: model_graph_code.mli *)
