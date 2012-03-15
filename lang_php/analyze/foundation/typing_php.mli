
exception UnknownEntity of string

(* 
 * Will process all functions and classes in env.code_database
 * in the topological order of their dependencies (bottom-up).
 * It will modify env by side effects, for instance env.genv will
 * contain the infered types for all functions and classes.
 *)
val infer_using_topological_sort_dependencies:
  Env_typing_php.env -> unit

val infer_using_topological_sort_dependencies_and_save_typingbin:
  Env_typing_php.env -> unit

val stmtl:
  Env_typing_php.env -> Ast_php_simple.program -> unit
(* used by unit testing *)
val func_def:
  Env_typing_php.env -> Ast_php_simple.func_def -> unit
val class_def:
  Env_typing_php.env -> Ast_php_simple.class_def -> unit

(* preparing env for infer_using_topological_sort_dependencies *)
val add_defs_code_database_and_update_dependencies: 
  Env_typing_php.env -> Ast_php_simple.program -> unit
