
type def = unit

type use = 
  Database_code.entity_kind * Ast_php.name

(* pre: unsugar_self_parent *)
val defs_of_any: Ast_php.any -> def list

(* pre: unsugar_self_parent *)
val uses_of_any: Ast_php.any -> use list
