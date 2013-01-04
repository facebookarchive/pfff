
type skip =
  (* mostly to avoid parsing errors messages *)
  | Dir of Common.dirname
  | File of Common.filename

  (* mostly for graph_code *)
  | Edge of string * string


val load: Common.filename -> skip list

val filter_files: 
  ?verbose:bool ->
  skip list -> Common.dirname (* root *) -> Common.filename list -> 
  Common.filename list

(* returns a hash leveraging the Hashtbl.find_all property *)
val build_filter_edges:
  skip list -> (string, string) Hashtbl.t
