(*s: export_ast_php.mli *)

(*s: json_ast_php.mli *)

(*s: json_ast_php flags *)
(*e: json_ast_php flags *)
(*
val json_string_of_program:  Ast_php.program  -> string
val json_string_of_toplevel: Ast_php.toplevel -> string
val json_string_of_expr:     Ast_php.expr     -> string
(* The outputted JSON is not pretty printed, it's more compact,
 * so less readable, but it's faster.
 *)
val json_string_of_program_fast:  Ast_php.program  -> string
*)
(*e: json_ast_php.mli *)

(*s: sexp_ast_php.mli *)

(*s: sexp_ast_php flags *)
(*e: sexp_ast_php flags *)
(*s: sexp_ast_php raw sexp *)
(*e: sexp_ast_php raw sexp *)
(*e: sexp_ast_php.mli *)
val ml_pattern_string_of_program:  Ast_php.program -> string
val ml_pattern_string_of_typehint: Ast_php.hint_type -> string
val ml_pattern_string_of_expr:     Ast_php.expr -> string
val ml_pattern_string_of_any:      Ast_php.any -> string
(*e: export_ast_php.mli *)
