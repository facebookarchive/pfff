(*s: checking_php.mli *)

(* Mainly checking the arity of function calls. Does a function call matches
 * the function prototype signature. Handle also static method calls
 * as it is easy to determine statically to what they correspond. For
 * methods see check_methods_php.ml
 *)
val check_program: 
  ?find_entity: Ast_entity_php.entity_finder option ->
  Ast_php.program -> unit

(*x: checking_php.mli *)
(*e: checking_php.mli *)
