
val draw_rectangle :
  cr:Cairo.t -> 
  color:Simple_color.emacs_color -> 
  Figures.rectangle -> unit

val draw_treemap_rectangle :
  cr:Cairo.t -> 
  ?color:Simple_color.emacs_color option -> ?alpha:float -> 
  Treemap.treemap_rectangle -> unit

val draw_treemap_rectangle_content_maybe :
  cr:Cairo.t ->
  clipping:Figures.rectangle ->
  nb_rects_on_screen: int ->
  model:Model2.model Model2.async ->
  settings:Model2.settings ->
  Treemap.treemap_rectangle -> unit

val draw_treemap_rectangle_label_maybe :
  cr:Cairo.t -> 
  zoom:float -> color:Simple_color.emacs_color -> 
  Treemap.treemap_rectangle -> unit

val text_with_user_pos :
  (string * Common.filepos * Cairo.point) Common.stack ref
