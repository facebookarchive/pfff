
exception ObsoleteConstruct of Ast_php.info
exception TodoConstruct of string * Ast_php.info

val store_position: bool ref

val program: 
  Ast_php.program -> Ast_php_simple.program
val program_with_position_information: 
  Ast_php.program -> Ast_php_simple.program

