
type skip =
  (* mostly to avoid parsing errors messages *)
  | Dir of Common.dirname
  | File of Common.filename

  (* mostly for graph_code *)
  | Edge of string * string

  | SkipErrorsDir of Common.dirname

val load: Common.filename -> skip list

val filter_files: 
  skip list -> Common.dirname (* root *) -> Common.filename list -> 
  Common.filename list

val reorder_files_skip_errors_last:
  skip list -> Common.dirname (* root *) -> Common.filename list -> 
  Common.filename list

(* returns a hash leveraging the Hashtbl.find_all property *)
val build_filter_edges:
  skip list -> (string, string) Hashtbl.t

(* returns true if we should skip the file for errors *)
val build_filter_errors_file:
  skip list -> (Common.filename (* readable *) -> bool)
