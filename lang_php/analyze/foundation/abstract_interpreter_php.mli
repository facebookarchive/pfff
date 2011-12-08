
exception UnknownFunction of string
exception UnknownMethod of string * string * string list
exception LostControl

val program:
  Env_interpreter_php.env -> Env_interpreter_php.heap ->
  Ast_php_simple.program ->
  Env_interpreter_php.heap

val extract_paths: bool ref
val graph: Env_interpreter_php.SSet.t Env_interpreter_php.SMap.t ref

(* used by unit testing *)
val _checkpoint_heap:
  (Env_interpreter_php.heap *
   Env_interpreter_php.value Env_interpreter_php.SMap.t) option ref


