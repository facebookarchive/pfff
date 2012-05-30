
type mvar = string 

type metavars_binding = (mvar, Ast_php.any) Common.assoc

val empty_environment: metavars_binding

val is_metavar_name: string -> bool
val is_metavar_variable_name: string -> bool
val is_metavar_lvalue_name: string -> bool

val metavar_regexp_string: string
val metavar_variable_regexp_string: string
val metavar_lvalue_regexp_string: string
