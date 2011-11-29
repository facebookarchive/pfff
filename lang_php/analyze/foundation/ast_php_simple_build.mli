
val program:
    Ast_php.program -> Ast_php_simple.program

val program_with_comments:
    Parser_php.token list ->
    Ast_php.program -> Ast_php_simple.program

(* intermediate functions used by cmf *)
type env = (int * Ast_php_simple.stmt) list ref

val empty_env: unit -> env
val func_def: env -> Ast_php.func_def -> Ast_php_simple.func_def
val class_def: env -> Ast_php.class_def -> Ast_php_simple.class_def
