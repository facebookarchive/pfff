
(* but right now only Expr and Stmt are supported *)
type pattern = Ast_php.any

val parse: string -> pattern
