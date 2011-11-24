
(* used by prettyphp *)
val program:
  (string -> unit) -> Ast_pp.program -> unit

(* used by test *)
val program_env:
  Pp2.env -> Ast_pp.program -> unit

(* used by spatch *)
val stmts:
  Pp2.env -> Ast_pp.stmt list -> unit

val class_elements:
  Pp2.env -> Ast_pp.class_element list -> unit

val class_header:
  Pp2.env -> Ast_pp.stmt list -> unit

val class_footer:
  Pp2.env -> unit -> unit

(* used by xhpize *)
val stmt:
  Pp2.env -> Ast_pp.stmt -> unit
