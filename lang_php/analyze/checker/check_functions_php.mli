(*s: checking_php.mli *)

(* Mainly checking the arity of function calls. Does a function call matches
 * the function prototype signature. Handle also static method calls
 * as it is easy to determine statically to what they correspond. For
 * methods see check_methods_php.ml
 *)
val check_program: 
  ?find_entity: Ast_entity_php.entity_finder option ->
  Ast_php.program -> unit

(* used also by check_classes_php.ml *)
val check_args_vs_params:
  (Ast_php.name * Ast_php.argument list) ->
  (Ast_php.name * Ast_php.parameter list) ->
  unit

(*x: checking_php.mli *)
(*e: checking_php.mli *)
