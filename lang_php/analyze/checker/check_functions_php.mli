(*s: checking_php.mli *)

(* Mainly checking the arity of function calls. Does a function call matches
 * the function prototype signature.
 *)
val check_program: Ast_php.program -> unit

(*x: checking_php.mli *)
(*e: checking_php.mli *)
