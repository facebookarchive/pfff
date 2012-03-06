
exception UnknownFunction of string
exception UnknownConstant of string
exception UnknownClass of string
exception UnknownMethod of string * string * string list
exception UnknownObject
exception LostControl

val strict: bool ref
val max_depth: int ref
val extract_paths: bool ref

module Interp: functor (Taint: Env_interpreter_php.TAINT) -> sig
  (* main entry point *)
  val program:
    Env_interpreter_php.env -> Env_interpreter_php.heap ->
    Ast_php_simple.program ->
    Env_interpreter_php.heap
end

(* used by unit testing *)
val _checkpoint_heap:
  (Env_interpreter_php.heap *
   Env_interpreter_php.value Env_interpreter_php.SMap.t) option ref

(* used by callgraph_php_build.ml *)
val graph: Callgraph_php2.callgraph ref


