
(* If the list returned is empty, then no match was found.
 * Note that [[]] means one match was found but the environment
 * is empty, for instance because your pattern does not contain any
 * metavariable.
 *)
type ('a, 'b) matcher = 
  'a -> 'b -> 
  Metavars_php.metavars_binding list

val match_e_e : (Ast_php.expr, Ast_php.expr) matcher
val match_v_v : (Ast_php.lvalue, Ast_php.lvalue) matcher
val match_st_st : (Ast_php.stmt, Ast_php.stmt) matcher

