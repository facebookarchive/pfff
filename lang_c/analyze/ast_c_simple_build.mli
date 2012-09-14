
exception ObsoleteConstruct of string * Ast_cpp.info
exception TodoConstruct of string * Ast_cpp.info

val program: 
  Ast_c.program -> Ast_c_simple.program
