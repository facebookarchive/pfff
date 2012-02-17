
(* only for Function, Class (merged with Interface),
 * Method (merged with StaticMethod), Constant for now. 
 * 
 * The 'name option' is For Method where it can be useful
 * to also have the name of the enclosing entity (this is useful in 
 * tags_php.ml for instance)
 *)
type def = 
  Database_code.entity_kind * Ast_php.name * Ast_php.name option

(* only for Function, Class uses for now *)
type use = 
  Database_code.entity_kind * Ast_php.name

val defs_of_any: Ast_php.any -> def list

(* pre: unsugar_self_parent *)
val uses_of_any: ?verbose:bool -> Ast_php.any -> use list
