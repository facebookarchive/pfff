
type ('a, 'b) transformer = 
  'a -> 'b -> 
  Metavars_php.metavars_binding list

(* this works by side effect on the second argument and its .transfo field *)
val transform_e_e :
  Ast_php.expr -> Ast_php.expr -> Metavars_php.metavars_binding -> unit
val transform_st_st : 
  Ast_php.stmt -> Ast_php.stmt  -> Metavars_php.metavars_binding -> unit
val transform_xhp_xhp : 
  Ast_php.xhp_html -> Ast_php.xhp_html  -> Metavars_php.metavars_binding -> unit
val transform_hint_hint :
  Ast_php.hint_type -> Ast_php.hint_type  -> Metavars_php.metavars_binding -> unit
