(*s: controller2.mli *)

val _refresh_da: (unit -> unit) ref
val _refresh_legend: (unit -> unit) ref

val _go_back: 
  (Model2.drawing ref -> unit) ref
val _go_dirs_or_file: 
  (?current_entity: Database_code.entity option ->
   ?current_grep_query: (string, Model2.line) Hashtbl.t option ->
    Model2.drawing ref -> Common.path list -> unit
  ) ref

val _statusbar_addtext: (string -> unit) ref
val _set_title: (string -> unit) ref

val current_rects_to_draw:
  (Treemap.treemap_rectangle list) ref
val current_r:
  Treemap.treemap_rectangle option ref

val paint_content_maybe_refresher:
  GMain.Idle.id option ref
val current_motion_refresher:
  GMain.Idle.id option ref

val dw_stack:
  Model2.drawing list ref

val title_of_path: string -> string

(*e: controller2.mli *)
