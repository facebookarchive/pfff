(*s: model_graph_code.mli *)
val build_uses_and_users_of_file: 
  Graph_code.graph ->
  (Common.filename * Common.filename list) list * (* uses_of_file *)
  (Common.filename * Common.filename list) list   (* husers_of_file *) 

(*e: model_graph_code.mli *)
