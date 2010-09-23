(*s: json_ast_php.mli *)

(*s: json_ast_php flags *)
(*e: json_ast_php flags *)

val string_of_program:  Ast_php.program  -> string
val string_of_toplevel: Ast_php.toplevel -> string
val string_of_expr:     Ast_php.expr     -> string

(* The outputted JSON is not pretty printed, it's more compact,
 * so less readable, but it's faster. 
 *)
val string_of_program_fast:  Ast_php.program  -> string
(*e: json_ast_php.mli *)
