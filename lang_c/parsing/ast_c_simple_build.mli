
exception ObsoleteConstruct of string * Parse_info.info
exception CplusplusConstruct
exception TodoConstruct of string * Parse_info.info

val program: 
  Ast_cpp.program -> Ast_c.program
