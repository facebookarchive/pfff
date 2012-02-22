
exception UnknownFunction of string
exception UnknownConstant of string
exception UnknownClass of string
exception UnknownMethod of string * string * string list
exception LostControl

val strict: bool ref
val max_depth: int ref

module type TAINT =
  sig
    val taint_mode : bool ref
    val taint_expr : 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val fold_slist :
      Env_interpreter_php.value list -> Env_interpreter_php.value
    val check_danger : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit
    val binary_concat :
      'a -> 'b -> 'c -> 'd -> 'e -> Env_interpreter_php.value
    module GetTaint :
      sig
        exception Found of string list
        val list : ('a -> unit) -> 'a list -> unit
        val value :
          Env_interpreter_php.heap ->
          Env_interpreter_php.value -> string list option
      end
    val when_call_not_found :
      Env_interpreter_php.heap ->
      Env_interpreter_php.value list -> Env_interpreter_php.value
  end

module Interp: functor (Taint: TAINT) -> sig
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
val extract_paths: bool ref
val graph: Callgraph_php2.callgraph ref


