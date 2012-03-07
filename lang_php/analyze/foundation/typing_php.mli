
exception UnknownEntity of string

(* will modify env by side effects, for instance env.genv will
 * contain the infered types for all functions and classes.
 *)
val program:
  Env_typing_php.env -> unit
val stmtl:
  Env_typing_php.env -> Ast_php_simple.program -> unit
(* used by unit testing *)
val func_def:
  Env_typing_php.env -> Ast_php_simple.func_def -> unit
val class_def:
  Env_typing_php.env -> Ast_php_simple.class_def -> unit


(* adding toplevel functions/classes in code database in env *)
val decls: 
  Env_typing_php.env -> Ast_php_simple.program -> unit

module Type : sig
    val unify: 
      Env_typing_php.env ->
      Env_typing_php.t -> Env_typing_php.t ->
      Env_typing_php.t
end
