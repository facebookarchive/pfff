
(* If the list returned is empty, then no match was found.
 * Note that [[]] means one match was found but the environment
 * is empty, for instance because your pattern does not contain any
 * metavariable.
 *)
type ('a, 'b) matcher = 
  'a -> 'b -> 
  Metavars_php.metavars_binding list

(* right now it does not side effect on the first argument but it could
 * at some point 
 *)
val match_e_e : (Ast_php.expr, Ast_php.expr) matcher
val match_v_v : (Ast_php.lvalue, Ast_php.lvalue) matcher
val match_st_st : (Ast_php.stmt, Ast_php.stmt) matcher
val match_top_top : (Ast_php.toplevel, Ast_php.toplevel) matcher
val match_xhp_xhp : (Ast_php.xhp_html, Ast_php.xhp_html) matcher

