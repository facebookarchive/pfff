
class context: 
  ctx: Dom_html.canvasRenderingContext2D Js.t -> 
object  
end

val draw_line: 
  Dom_html.canvasRenderingContext2D Js.t -> 
  string * float * (float * float) * (float * float) ->
  unit

type css_color = string

val rgba_of_rgbf: 
  Simple_color.rgbf -> float (* alpha *) -> css_color
