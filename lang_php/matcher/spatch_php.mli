
(* right now only Expr and Stmt are supported *)
type pattern = Ast_php.any

val parse: 
 Common.filename -> pattern
val parse_string: 
 string -> pattern

val spatch: 
  ?case_sensitive: bool ->
  pattern -> Common.filename -> string option

