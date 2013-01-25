
val parse: 
  Common.filename -> (Parse_cpp.program2 * Statistics_parsing.parsing_stat)

val parse_program:
  Common.filename -> Ast_objc.program
