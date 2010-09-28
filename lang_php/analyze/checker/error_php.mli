
val strict: bool ref

type error = 
  | TooManyArguments of (Parse_info.parse_info * Ast_php.name (* def *))
  | NotEnoughArguments of (Parse_info.parse_info * Ast_php.name (* def *))

  | UseOfUndefinedVariable of Ast_php.dname
  | UseOfUndefinedMember of Ast_php.name
  | UnusedVariable of Ast_php.dname  * Scope_php.phpscope

val string_of_error: error -> string

exception Error of error

val report_error : error -> unit

val _errors: error list ref

val fatal: error -> unit
val warning: error -> unit

val report_all_errors: unit -> unit

