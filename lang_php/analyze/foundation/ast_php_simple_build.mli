
exception ObsoleteConstruct of Ast_php.info
exception TodoConstruct of string * Ast_php.info

val store_position: bool ref

val program: 
  Ast_php.program -> Ast_php_simple.program
val program_with_position_information: 
  Ast_php.program -> Ast_php_simple.program

(* intermediate functions used by cmf *)
val func_def: Ast_php.func_def -> Ast_php_simple.func_def
val class_def: Ast_php.class_def -> Ast_php_simple.class_def
val constant_def: Ast_php.constant_def -> Ast_php_simple.constant_def
