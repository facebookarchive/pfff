(*

type cpp_option = 
  | I of Common.dirname
  | D of string * string option

val cpp_option_of_cmdline: 
  Common.dirname list (* -I *) * string list (* -D *) -> cpp_option list
val show_cpp_i_opts: string list -> unit
val show_cpp_d_opts: string list -> unit

(* #include *)
val cpp_expand_include: 
  ?depth_limit:int option ->
  ?threshold_cache_nb_files:int ->
  cpp_option list -> Common.dirname (* start point for relative paths *) -> 
  Ast_c.program -> Ast_c.program

(* #ifdef *)
val cpp_ifdef_statementize: 
  Ast_c.program -> Ast_c.program

(* #define *)
val cpp_expand_macro_expr: 
  Ast_c.define_kind -> Ast_c.argument Ast_c.wrap2 list -> 
  Ast_c.expression option
*)
