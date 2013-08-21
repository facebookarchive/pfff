
type location_elt =
  | File of Common.filename * int (* line *) * int (* col *)
  | Line of int (* line *) * int (* col *)
  | Col of int
  | Other

val locations_of_angle:
  int * Common.filename (* for error report, line x file in .clang *) -> 
  Ast_clang.sexp list -> location_elt list

val locations_of_paren:
  Common.filename (* for error report, line x file in .clang *) ->
  (Ast_clang.enum * int * Ast_clang.sexp list) -> location_elt list


val readable_of_filename:
  root:Common.dirname ->
  Common.filename -> Common.filename

val readable_filename_location_of_paren_opt:
  root:Common.dirname ->
  Common.filename ->
  (Ast_clang.enum * int * Ast_clang.sexp list) -> 
  Common.filename option

val unknown_loc_angle: Ast_clang.sexp

val str_of_angle_loc:
  int -> Ast_clang.sexp -> Common.filename -> string
