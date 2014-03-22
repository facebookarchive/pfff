
type error = {
  typ: error_kind;
  loc: Parse_info.token_location;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind =
 | Deadcode of (string * Database_code.entity_kind)
 | UndefinedDefOfDecl of (string * Database_code.entity_kind)

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

(* have a few false positives in scheck so filter them *)
val adjust_errors:
  error list -> error list
