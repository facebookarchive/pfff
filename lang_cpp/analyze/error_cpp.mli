type error = {
  typ: error_kind;
  loc: Parse_info.info;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind =
  | UnusedVariable of string * Scope_code.scope

val string_of_error_kind: error_kind -> string

(* ugly global, but sometimes they are practical *)
val _errors: error list ref

val fatal: Parse_info.info -> error_kind -> unit
val warning: Parse_info.info -> error_kind -> unit
