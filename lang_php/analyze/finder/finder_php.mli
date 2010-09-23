(*s: finder_php.mli *)

(* position based finder *)
val info_at_pos: int -> Ast_php.toplevel -> Ast_php.info
val expr_at_pos: int -> Ast_php.toplevel -> Ast_php.expr

val info_at_pos_in_full_program: int -> Ast_php.program -> Ast_php.info
(*x: finder_php.mli *)
(*e: finder_php.mli *)
