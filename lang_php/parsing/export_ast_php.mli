(*s: export_ast_php.mli *)

val ml_pattern_string_of_program: 
  Ast_php.program -> string

val ml_pattern_string_of_expr: 
  Ast_php.expr -> string

val ml_pattern_string_of_any: 
  Ast_php.any -> string
(*e: export_ast_php.mli *)
