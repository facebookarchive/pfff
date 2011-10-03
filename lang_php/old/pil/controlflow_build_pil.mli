
val cfg_of_stmts: Pil.stmt list -> Controlflow_pil.flow

type error = error_kind * Ast_php.info
 and error_kind = 
  | NoEnclosingLoop
  | DynamicBreak

val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

exception Error of error

val report_error : error -> unit
