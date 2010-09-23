(*s: database_php_statistics.mli *)

val parsing_stat_db:   Database_php.database -> unit
val typing_stat_db:    Database_php.database -> unit
(*
val callgraph_stat_db: Database_php.database -> unit
val extra_stat_db:     Database_php.database -> unit
*)
val fields_stat_db: Database_php.database -> unit
val includes_stat_db: Database_php.database -> unit

val all_stat_db:      Database_php.database -> unit

val actions: unit -> Common.cmdline_actions
(*x: database_php_statistics.mli *)
(*e: database_php_statistics.mli *)
