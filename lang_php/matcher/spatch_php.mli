
(* but right now only Expr and Stmt are supported *)
type pattern = Ast_php.any

val parse_spatch: Common.filename -> pattern

val spatch: pattern -> Common.filename -> string option

