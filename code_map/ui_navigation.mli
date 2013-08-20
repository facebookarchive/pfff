(*s: ui_navigation.mli *)

val go_back: 
  Model2.drawing ref -> unit

val go_dirs_or_file:
  ?current_entity:Database_code.entity option ->
  ?current_grep_query:(Common.filename, Model2.line) Hashtbl.t option ->
  Model2.drawing ref -> Common.filename list -> unit

(*e: ui_navigation.mli *)
