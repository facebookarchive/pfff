
(* configuration *)
val strict: bool ref
val max_depth: int ref
val extract_paths: bool ref

(* exceptions thrown when in strict mode *)
exception UnknownFunction of string
exception UnknownClass of string
exception UnknownConstant of string

exception UnknownMethod of string * string * string list
exception UnknownObject

exception LostControl

module Interp: functor (Taint: Env_interpreter_php.TAINT) -> sig
  (* Main entry point. Its main use is to call Taint.check_danger
   * when it finds XSS holes. Another use it to generate
   * a callgraph (see graph below) by side effect when
   * extract_paths is true. 
   * 
   * todo: It could be used also to find general bugs that
   * the current checker can't (e.g. undefined methods because
   * of the better class analysis, wrong type, etc). 
   * todo: It could also be used for program understanding purpose
   * by providing a kind of tracer.
   *)
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
