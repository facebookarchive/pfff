
type context = Dom_html.canvasRenderingContext2D Js.t

val draw_line: 
  context -> 
  string * float * (float * float) * (float * float) ->
  unit

type css_color = string

val rgba_of_rgbf: 
  Simple_color.rgbf -> float (* alpha *) -> css_color
