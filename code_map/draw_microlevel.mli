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
(* ugly: used when middle-clicking on the drawing area to know 
 * how to translate a point into a filepos so that we can open
 * the file at the right position.
 *)
val text_with_user_pos:
  (string * Common2.filepos * Cairo.point) Common.stack ref
(*e: text_with_user_pos sig *)

(*e: draw_microlevel.mli *)
