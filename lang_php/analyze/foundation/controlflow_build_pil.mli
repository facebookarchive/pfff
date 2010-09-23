
val cfg_of_stmts: Pil.stmt list -> Controlflow_pil.flow

type error = 
  | NoEnclosingLoop of Ast_php.info
  | DynamicBreak    of Ast_php.info

exception Error of error

val report_error : error -> unit
