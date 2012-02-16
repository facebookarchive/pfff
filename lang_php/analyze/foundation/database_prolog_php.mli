
val gen_prolog_db:
  ?show_progress:bool ->
  Database_php.database -> Common.filename -> unit

val append_callgraph_to_prolog_db:
  ?show_progress:bool ->
  Database_php.database -> Common.filename -> unit
