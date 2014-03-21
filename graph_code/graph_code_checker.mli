
type error = {
  typ: error_kind;
  loc: Parse_info.info;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind =
 | Deadcode

val string_of_error_kind: error_kind -> string
val string_of_error: error -> string

val g_errors: error list ref

val fatal: Parse_info.info -> error_kind -> unit
val warning: Parse_info.info -> error_kind -> unit

(* modify _errors *)
val check: Common.dirname -> Graph_code.graph -> unit
