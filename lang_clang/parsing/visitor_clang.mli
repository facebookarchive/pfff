
type visitor = 
  (Ast_clang.sexp -> unit) -> Ast_clang.sexp -> unit

val visit: visitor -> Ast_clang.sexp -> unit
