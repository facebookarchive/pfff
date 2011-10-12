(*s: checking_php.mli *)

(* Checking the use of class variables, class constants, and class names.
 * Checking method calls arity is actually done in check_functions_php.ml.
 *)
val check_program: 
  Entity_php.entity_finder -> Ast_php.program -> unit

(*x: checking_php.mli *)
(*e: checking_php.mli *)
