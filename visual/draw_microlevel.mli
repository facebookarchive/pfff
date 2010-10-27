(*s: draw_microlevel.mli *)

(*s: draw_treemap_rectangle_content_maybe sig *)
val draw_treemap_rectangle_content_maybe :
  cr:Cairo.t ->
  clipping:Figures.rectangle ->
  context:Model2.context ->
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle_content_maybe sig *)

(*s: text_with_user_pos sig *)
(* ugly: used when middle-clicking on the drawing area to know 
 * how to translate a point into a filepos so that we can open
 * the file at the right position.
 *)
val text_with_user_pos :
  (string * Common.filepos * Cairo.point) Common.stack ref
(*e: text_with_user_pos sig *)

(* current used also by draw_macrolevel *)
val final_font_size_when_multiplier:
  multiplier:float ->
  size_font_multiplier_multiplier:float ->
  font_size:float -> font_size_real:'a -> float

(*e: draw_microlevel.mli *)
