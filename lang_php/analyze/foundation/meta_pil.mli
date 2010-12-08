
(* for debugging *)
type debug_config = {
  show_types: bool;
  show_tokens: bool;
}
val default_debug_config: debug_config

val string_of_instr: ?config:debug_config -> Pil.instr -> string
val string_of_stmt: ?config:debug_config -> Pil.stmt -> string
val string_of_expr: ?config:debug_config -> Pil.expr -> string

val string_of_program: ?config:debug_config -> Pil.program -> string

(* meta *)
val vof_expr: Pil.expr -> Ocaml.v
val vof_instr: Pil.instr -> Ocaml.v
val vof_argument: Pil.argument -> Ocaml.v

(* meta *)
val vof_node: Controlflow_pil.node -> Ocaml.v
