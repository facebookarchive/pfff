(*s: database_php_build2.mli *)
(* If the environment is empty then use the project path stored in
 * the database for PHP_ROOT.
 *)
val index_db_includes_requires:
  ?hook_additional_includes:
  (Common.filename -> Ast_php.program -> Common.filename list) ->
  Env_php.env option ->
  Database_php.database ->
  unit

val actions: unit -> Common.cmdline_actions

(*e: database_php_build2.mli *)
