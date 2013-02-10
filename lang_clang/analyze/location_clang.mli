
type location =
  | File of Common.filename * int * int
  | Line of int * int
  | Col of int
  | Other

val location_of_angle:
  int * string -> Ast_clang.sexp list -> location list

val readable_of_filename:
  Common.filename -> Common.filename

val location_of_paren_opt:
  Common.filename ->
  (Ast_clang.enum * int * Ast_clang.sexp list) -> 
  Common.filename option
