(*s: typing_trivial_php.mli *)

(* assume an xdebug-expr, that is an expr with mostly concrete scalars *)
val type_of_expr: Ast_php.expr -> Type_php.phptype

(* may return an exception if not "union-able" *)
val union_type: Type_php.phptype -> Type_php.phptype -> Type_php.phptype

val is_normalized: Type_php.phptype -> bool

(*x: typing_trivial_php.mli *)
(*e: typing_trivial_php.mli *)
