
type precision = {
  full_info: bool;
  token_info: bool;
  type_info: bool;
}
val default_precision: precision

val vof_program: precision -> Ast_js.program -> Ocaml.v
