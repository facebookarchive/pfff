
val protected_to_private: Graph_code.graph -> unit

val build_uses_and_users_of_file: 
  Graph_code.graph ->
  (Common.filename, Common.filename list) Hashtbl.t * (* huses_of_file *)
  (Common.filename, Common.filename list) Hashtbl.t   (* husers_of_file *) 



