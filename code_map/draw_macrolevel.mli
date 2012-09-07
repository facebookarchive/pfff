(*s: draw_macrolevel.mli *)

(*s: draw_treemap_rectangle sig *)
val draw_treemap_rectangle :
  cr:Cairo.t -> 
  ?color:Simple_color.emacs_color option -> 
  ?alpha:float -> 
  Treemap.treemap_rectangle -> 
  unit
(*e: draw_treemap_rectangle sig *)

val draw_trect_using_layers:
  cr:Cairo.t ->
  Layer_code.layers_with_index ->
  Treemap.treemap_rectangle -> 
  unit

(*e: draw_macrolevel.mli *)
