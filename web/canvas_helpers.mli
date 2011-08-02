
type context = Dom_html.canvasRenderingContext2D Js.t

val draw_line: 
  context -> 
  string * float * (float * float) * (float * float) ->
  unit
