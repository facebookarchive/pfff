(*s: unparse_php.mli *)

val string_of_program2: Parse_php.program2 -> string

(* for now it does not maintain comments or space *)
val string_of_any: Ast_php.any -> string

val string_of_expr: Ast_php.expr -> string

val string_of_infos: Ast_php.info list -> string

val string_of_program2_using_tokens: 
  ?remove_space_after_removed:bool ->
  Parse_php.program2 -> string

(*e: unparse_php.mli *)
