(*s: draw_microlevel.mli *)

(*s: draw_treemap_rectangle_content_maybe sig *)
(* will render (maybe) the file content of treemap_rectangle.tr_label *)
val draw_treemap_rectangle_content_maybe:
  cr:Cairo.t ->
  clipping:Figures.rectangle ->
  context:Model2.context ->
  Treemap.treemap_rectangle -> 
  Model2.microlevel option
(*e: draw_treemap_rectangle_content_maybe sig *)

(*s: text_with_user_pos sig *)
(*e: text_with_user_pos sig *)

val draw_magnify_line:
  Cairo.t -> Model2.line -> Model2.microlevel -> unit
(*e: draw_microlevel.mli *)
