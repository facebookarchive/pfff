(*s: pil_build.mli *)

val linearize_expr: Ast_php.expr -> Pil.instr list * Pil.expr

val linearize_stmt: Ast_php.stmt -> Pil.stmt list

val linearize_body: Ast_php.stmt_and_def list -> Pil.stmt list

(*e: pil_build.mli *)
