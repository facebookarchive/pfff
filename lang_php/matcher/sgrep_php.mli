
(* but right now only Expr and Stmt are supported *)
type pattern = Ast_php.any

val parse: 
  string -> pattern

val sgrep:   
  ?case_sensitive: bool ->
  hook:(Metavars_php.metavars_binding -> Ast_php.info list -> unit) ->
  Ast_php.any -> Common.filename -> unit

val sgrep_ast:
  ?case_sensitive: bool ->
  hook:(Metavars_php.metavars_binding -> Ast_php.info list -> unit) ->
  Ast_php.any -> Ast_php.program -> unit
