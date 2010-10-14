(*s: unparse_php.mli *)

val string_of_program2: Parse_php.program2 -> string

val string_of_toplevel: Ast_php.toplevel -> string

(* for now it does not maintain comments or space *)
val string_of_expr: Ast_php.expr -> string
val string_of_lvalue: Ast_php.lvalue -> string
val string_of_param: Ast_php.parameter -> string

val string_of_infos: Ast_php.info list -> string

val string_of_program2_using_tokens: 
  ?remove_space_after_removed:bool ->
  Parse_php.program2 -> string

(*e: unparse_php.mli *)
