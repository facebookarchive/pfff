
type error = {
  typ: error_kind;
  loc: Parse_info.token_location;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind =
 | Deadcode of entity
 | UndefinedDefOfDecl of entity
 | UnusedExport of entity * Common.filename
 | UnusedVariable of string * Scope_code.scope

 and entity = (string * Entity_code.entity_kind)


(* @xxx to acknowledge or explain false positives *)
type annotation =
  | AtScheck of string

(* to detect false positives (we use the Hashtbl.find_all property) *)
type identifier_index = (string, Parse_info.token_location) Hashtbl.t


val string_of_error: error -> string
val string_of_error_kind: error_kind -> string


val g_errors: error list ref
(* !modify g_errors! *)
val fatal: Parse_info.token_location -> error_kind -> unit
val warning: Parse_info.token_location -> error_kind -> unit

type rank =
 | Never
 | OnlyStrict
 | Less
 | Ok
 | Important
 | ReallyImportant

val score_of_rank: 
  rank -> int
val rank_of_error:
  error -> rank
val score_of_error:
  error -> int

val annotation_at:
  Parse_info.token_location -> annotation option

(* have some approximations and Fps in graph_code_checker so filter them *)
val adjust_errors:
  error list -> error list
