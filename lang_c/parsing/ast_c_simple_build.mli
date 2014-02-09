
exception ObsoleteConstruct of string * Ast_cpp.info
exception CplusplusConstruct
exception TodoConstruct of string * Ast_cpp.info

val program: 
  Ast_cpp.program -> Ast_c.program
