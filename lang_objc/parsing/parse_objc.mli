
val parse: 
  Common.filename -> (Parse_cpp.program2 * Parse_info.parsing_stat)

val parse_program:
  Common.filename -> Ast_objc.program
