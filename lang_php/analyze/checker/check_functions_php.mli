(*s: checking_php.mli *)

(* Mainly checking the arity of function calls. Does a function call matches
 * the function prototype "signature". Handle also static method calls
 * as it is easy to determine statically to what they correspond. Also
 * handle method calls using $this-> as it's also easy to determine statically
 * to what they correspond.
 * 
 * pre: program without self/parent
 * 
 * todo: not only check arity but types ...
 *)
val check_program: 
  Entity_php.entity_finder -> Ast_php.program -> unit

(* used also by check_classes_php.ml to check new Xxx() calls *)
val check_args_vs_params:
  (Ast_php.name * Ast_php.argument list) ->
  (Ast_php.ident * Ast_php.parameter list) ->
  unit
val contain_func_name_args_like: Ast_php.any -> bool
(*x: checking_php.mli *)
(*e: checking_php.mli *)
