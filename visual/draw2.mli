(*s: draw2.mli *)

(*s: type context *)
(* a slice of Model2.drawing *)
type context = {
  model: Model2.model Model2.async;
  settings:Model2.settings;
  nb_rects_on_screen: int;
  current_grep_query: (Common.filename, int) Hashtbl.t;
}
(*e: type context *)

(*s: draw_treemap_rectangle sig *)
val draw_treemap_rectangle :
  cr:Cairo.t -> 
  ?color:Simple_color.emacs_color option -> 
  ?alpha:float -> 
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle sig *)

(*s: draw_treemap_rectangle_content_maybe sig *)
val draw_treemap_rectangle_content_maybe :
  cr:Cairo.t ->
  clipping:Figures.rectangle ->
  context:context ->
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle_content_maybe sig *)

(*s: draw_treemap_rectangle_label_maybe sig *)
val draw_treemap_rectangle_label_maybe :
  cr:Cairo.t -> 
  zoom:float -> 
  color:Simple_color.emacs_color -> 
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle_label_maybe sig *)

(*s: text_with_user_pos sig *)
(* ugly: used when middle-clicking on the drawing area to know 
 * how to translate a point into a filepos so that we can open
 * the file at the right position.
 *)
val text_with_user_pos :
  (string * Common.filepos * Cairo.point) Common.stack ref
(*e: text_with_user_pos sig *)

(*e: draw2.mli *)
