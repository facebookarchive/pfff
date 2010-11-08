
type mvar = string 

type metavars_binding = (mvar, Ast_php.any) Common.assoc

val empty_environment: metavars_binding
