
val ml_pattern_string_of_program: 
  ?precision:Meta_ast_generic.precision ->
  Ast_cpp.program  -> string

val ml_pattern_string_of_any: 
  ?precision:Meta_ast_generic.precision ->
  Ast_cpp.any  -> string

val string_json_of_program:  Ast_cpp.program  -> string

