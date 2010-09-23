
type mvar = string 

type metavars_binding = (mvar, binded_code) Common.assoc
 and binded_code =
   | Expr of Ast_php.expr

val empty_environment: metavars_binding
