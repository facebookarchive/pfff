(*s: controlflow_build_php.mli *)

(*s: controlflow builders signatures *)
val control_flow_graph_of_stmts: Ast_php.stmt list -> Controlflow_php.flow

(* alias *)
val cfg_of_stmts: Ast_php.stmt list -> Controlflow_php.flow

val cfg_of_func:   Ast_php.func_def   -> Controlflow_php.flow
val cfg_of_method: Ast_php.method_def -> Controlflow_php.flow
(*e: controlflow builders signatures *)

(*s: controlflow checkers signatures *)
val deadcode_detection : Controlflow_php.flow -> unit
(*e: controlflow checkers signatures *)

(*s: type Controlflow_build_php.error *)
type error = 
  | DeadCode        of Ast_php.info
  | NoEnclosingLoop of Ast_php.info
  | ColonSyntax     of Ast_php.info
  | NoMethodBody    of Ast_php.info
  | DynamicBreak    of Ast_php.info
(*e: type Controlflow_build_php.error *)

val string_of_error: error -> string
val info_of_error: error -> Ast_php.info option

(*s: error exception and report_error signature *)
exception Error of error

val report_error : error -> unit
(*e: error exception and report_error signature *)
(*x: controlflow_build_php.mli *)
(*e: controlflow_build_php.mli *)
