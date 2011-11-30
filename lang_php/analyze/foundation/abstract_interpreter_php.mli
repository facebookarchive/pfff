
val extract_paths: bool ref
val graph: Env_interpreter_php.SSet.t Env_interpreter_php.SMap.t ref

val program:
  Env_interpreter_php.env -> Env_interpreter_php.heap ->
  Ast_php_simple.program ->
  Env_interpreter_php.heap

(* used by unit testing *)
val _checkpoint_heap:
  (Env_interpreter_php.heap *
   Env_interpreter_php.value Env_interpreter_php.SMap.t) option ref


