(*s: checking_php.mli *)

(* Mainly checking if a class is defined. See check_methods_php.ml for
 * more complicated and useful analysis
 *)
val check_program: 
  ?find_entity: Entity_php.entity_finder option ->
  Ast_php.program -> unit

(*x: checking_php.mli *)
(*e: checking_php.mli *)
