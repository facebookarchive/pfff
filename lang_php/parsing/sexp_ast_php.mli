(*s: sexp_ast_php.mli *)

(*s: sexp_ast_php flags *)
val show_info:      bool ref
val show_expr_info: bool ref
val show_annot: bool ref
(*e: sexp_ast_php flags *)

val string_of_program:  Ast_php.program  -> string
val string_of_toplevel: Ast_php.toplevel -> string
val string_of_expr:     Ast_php.expr     -> string
val string_of_phptype:  Type_php.phptype  -> string

(*s: sexp_ast_php raw sexp *)
(*e: sexp_ast_php raw sexp *)
(*e: sexp_ast_php.mli *)
