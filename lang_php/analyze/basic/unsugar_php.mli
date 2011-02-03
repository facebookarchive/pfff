
(* post: the Self and Parent constructors are not anymore possible cases *)
val unsugar_self_parent_any: Ast_php.any -> Ast_php.any

(* special cases *)
val unsugar_self_parent_program: Ast_php.program -> Ast_php.program
val unsugar_self_parent_toplevel: Ast_php.toplevel -> Ast_php.toplevel
