
type location =
  | File of Common.filename * int * int
  | Line of int * int
  | Col of int
  | Other

val location_of_angle:
  int * string -> Ast_clang.sexp list -> location list

val readable_of_filename:
  root:Common.dirname ->
  Common.filename -> Common.filename

val location_of_paren_opt:
  root:Common.dirname ->
  Common.filename ->
  (Ast_clang.enum * int * Ast_clang.sexp list) -> 
  Common.filename option

val unknown_loc_angle: Ast_clang.sexp

val str_of_angle_loc:
  int -> Ast_clang.sexp -> Common.filename -> string
