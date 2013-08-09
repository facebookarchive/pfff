(*s: model_graph_code.mli *)
val build_uses_and_users_of_file: 
  Graph_code.graph ->
  (Common.filename, Common.filename list) Common.assoc * (* uses_of_file *)
  (Common.filename, Common.filename list) Common.assoc   (* husers_of_file *) 

val build_entities_of_file:
  Graph_code.graph ->
  (Common.filename, (int * Database_code.entity) list) Common.assoc
(*e: model_graph_code.mli *)
