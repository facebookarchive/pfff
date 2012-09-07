(*s: ui_search.mli *)

val dialog_search_def: 
  Model2.model Async.t -> string option

val run_grep_query:
  root:string -> string -> (string * int) list

val run_tbgs_query:
  root:string -> string -> (string * int) list

(*e: ui_search.mli *)
