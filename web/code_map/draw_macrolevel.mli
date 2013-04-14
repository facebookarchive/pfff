
val draw_treemap_rectangle:
  Dom_html.canvasRenderingContext2D Js.t ->
  ?color:Simple_color.emacs_color option -> 
  ?alpha:float ->
  Treemap.treemap_rectangle ->
  unit
