(*s: database_php_build.mli *)

(* in helpers now: val _errors: string list ref *)

val max_phase: int

(* main entry point, does the whole job *)
val create_db : 
  ?verbose_stats: bool ->
  ?db_support:Database_php.db_support ->
  ?phase:int -> 
  ?files:Common.filename list option ->
  (* only if want to go to phase 4 *)
  annotate_variables_program:
    (find_entity:Entity_php.entity_finder option -> Ast_php.program -> unit)
    option ->
  Database_php.project -> 
  Database_php.database
(* see also database_php_build2.mli for other analysis (e.g. method analysis *)

(* will use Include_require_php.recursive_included_files_of_file *)
val fast_create_db_mem_a_la_cpp: 
  ?phase:int -> Common.path list -> Database_php.database

(* helpers used also in unit_analyze_php.ml *)
val db_of_files_or_dirs: 
  ?annotate_variables_program:
    (find_entity:Entity_php.entity_finder option -> Ast_php.program -> unit)
    option ->
  Common.path list -> Database_php.database

(* wrapper that makes it easier for some code to not depend on database_php *)
val build_entity_finder: 
  Database_php.database -> Entity_php.entity_finder

val actions: unit -> Common.cmdline_actions

(*x: database_php_build.mli *)
(*e: database_php_build.mli *)
