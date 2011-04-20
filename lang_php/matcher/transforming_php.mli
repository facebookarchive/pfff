
type ('a, 'b) transformer = 
  'a -> 'b -> 
  Metavars_php.metavars_binding list

(* this works by side effect on the second argument and its .transfo field *)
val transform_e_e :
  Ast_php.expr -> Ast_php.expr -> Metavars_php.metavars_binding -> unit
val transform_st_st : 
  Ast_php.stmt -> Ast_php.stmt  -> Metavars_php.metavars_binding -> unit
val transform_v_v :
  Ast_php.lvalue -> Ast_php.lvalue -> Metavars_php.metavars_binding -> unit

(* 
val match_e_e : (Ast_php.expr, Ast_php.expr) matcher
val match_v_v : (Ast_php.lvalue, Ast_php.lvalue) matcher 
*)

