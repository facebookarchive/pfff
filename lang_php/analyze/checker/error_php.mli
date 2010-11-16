
val strict: bool ref

type error = 
  | UndefinedFunction of Ast_php.name
  | UnableToDetermineDef of Ast_php.name

  | TooManyArguments of (Parse_info.parse_info * Ast_php.name (* def *))
  | NotEnoughArguments of (Parse_info.parse_info * Ast_php.name (* def *))

  | TooManyArguments2 of Ast_php.name * Ast_php.func_def
  | TooFewArguments2  of Ast_php.name * Ast_php.func_def
  | WrongKeywordArgument of Ast_php.dname * Ast_php.expr * Ast_php.name *
                     Ast_php.parameter * Ast_php.func_def

  | UseOfUndefinedVariable of Ast_php.dname
  | UnusedVariable of Ast_php.dname  * Scope_php.phpscope

  | UseOfUndefinedMember of Ast_php.name

val string_of_error: error -> string

exception Error of error

val report_error : error -> unit

val _errors: error list ref

val fatal: error -> unit
val warning: error -> unit

val report_all_errors: unit -> unit

