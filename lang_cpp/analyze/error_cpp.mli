type error = {
  typ: error_kind;
  loc: Ast_cpp.info;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind =
  | UnusedVariable of string * Scope_code.scope

val string_of_error_kind: error_kind -> string

(* ugly global, but sometimes they are practical *)
val _errors: error list ref

val fatal: Ast_cpp.info -> error_kind -> unit
val warning: Ast_cpp.info -> error_kind -> unit
