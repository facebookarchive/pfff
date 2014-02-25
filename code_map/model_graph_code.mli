(*s: model_graph_code.mli *)
val build_deps_of_file: 
  Graph_code.graph ->
  (Common.filename, Common.filename list) Common.assoc * (* uses *)
  (Common.filename, Common.filename list) Common.assoc   (* users *) 

(* the nodes are sorted by line numbers *)
val build_entities_of_file:
  Graph_code.graph ->
  (Common.filename, Graph_code.node list) Common.assoc

val add_headers_files_entities_of_file:
  Common.dirname -> 
  (Common.filename, Graph_code.node list) Common.assoc ->
  (Common.filename, Graph_code.node list) Common.assoc
(*e: model_graph_code.mli *)
