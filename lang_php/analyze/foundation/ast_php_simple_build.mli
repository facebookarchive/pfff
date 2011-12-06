
val program: Ast_php.program -> Ast_php_simple.program

(* intermediate functions used by cmf *)
val func_def: Ast_php.func_def -> Ast_php_simple.func_def
val class_def: Ast_php.class_def -> Ast_php_simple.class_def
