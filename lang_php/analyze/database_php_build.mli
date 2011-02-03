(*s: database_php_build.mli *)

val _errors: string list ref

(* main entry point, does the whole job *)
val create_db : 
  ?verbose_stats: bool ->
  ?db_support:Database_php.db_support ->
  ?phase:int -> 
  ?files:Common.filename list option ->
  Database_php.project -> 
  Database_php.database

(* use Include_require_php.recursive_included_files_of_file *)
val fast_create_db_mem:
  ?phase:int ->
  Common.path list -> 
  Database_php.database

(* wrapper that makes it easier for some code to not depend on database_php *)
val build_entity_finder: 
  Database_php.database -> Entity_php.entity_finder

val actions: unit -> Common.cmdline_actions

val max_phase: int

(* see also database_php_build2.mli for other analysis (e.g. method analysis *)


(* helpers used internally that can be useful to other *)
val iter_files: 
  Database_php.database -> 
  ((Common.filename * Database_php.id list) * 
  int (* idx *) * int (* total *) -> unit) ->
  unit

val iter_files_and_topids :
  Database_php.database -> string -> 
  (Database_php.id -> Common.filename -> unit) -> 
  unit

val iter_files_and_ids :
  Database_php.database -> string -> 
  (Database_php.id -> Common.filename -> unit) -> 
  unit
(*x: database_php_build.mli *)
(*e: database_php_build.mli *)
