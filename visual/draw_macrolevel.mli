(*s: draw_macrolevel.mli *)

(*s: draw_treemap_rectangle sig *)
val draw_treemap_rectangle :
  cr:Cairo.t -> 
  ?color:Simple_color.emacs_color option -> 
  ?alpha:float -> 
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle sig *)

(*e: draw_macrolevel.mli *)
