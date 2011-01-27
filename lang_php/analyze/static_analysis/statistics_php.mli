(*s: statistics_php.mli *)
type stat = {
  mutable functions: int;
  mutable classes: int;
  mutable toplevels_funcalls: int;
  mutable toplevels_assign: int;
  mutable toplevels_other: int;
  mutable toplevels_include_requires: int;

  mutable toplevels_funcalls_kinds: (string * int) list;
  (* toplevels_assign_kinds? *)
}
type php_file_kind =
  | LibFile
  | IncluderFile
  | ScriptOrEndpointFile

val default_stat: unit -> stat

val string_of_stat: stat -> string

val stat_of_program: Ast_php.program -> stat

(* helpers *)
val kind_of_file_using_stat: stat -> php_file_kind

(*x: statistics_php.mli *)
(*e: statistics_php.mli *)
