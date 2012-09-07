(*s: draw_labels.mli *)

(*s: draw_treemap_rectangle_label_maybe sig *)
val draw_treemap_rectangle_label_maybe :
  cr:Cairo.t -> 
  zoom:float -> 
  color:Simple_color.emacs_color option -> 
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle_label_maybe sig *)

(*e: draw_labels.mli *)
