
type css_color = string

val css_color_of_rgbf: 
  Simple_color.rgbf -> float (* alpha *) -> css_color

(* todo: make it class type in h_visualization/ to abstract away differences
 * between gtk and canvas
 *)
class context: 
  ctx: Dom_html.canvasRenderingContext2D Js.t -> 
  width: int -> height: int ->
  xy_ratio: float ->
object  
  method orig_coord_width: float
  method orig_coord_height: float 

  method canvas_ctx: Dom_html.canvasRenderingContext2D Js.t

  method fillStyle: ?alpha:float -> Simple_color.emacs_color -> unit
  method strokeStyle: ?alpha:float -> Simple_color.emacs_color -> unit

  method device_to_user: x:int -> y:int -> float * float
  method device_to_user_size: int -> float

(*
  method draw_line: (css_color * float * (float * float) * (float * float)) 
    -> unit
*)

  method draw_rectangle:
    ?alpha:float -> color:Simple_color.emacs_color ->
    line_width:float -> 
    Figures.rectangle -> unit

  method fill_rectangle:
    ?alpha:float -> color:Simple_color.emacs_color -> 
    Figures.rectangle -> unit
  method fill_rectangle_xywh:
    ?alpha:float ->
    x:float -> y:float ->
    w:float -> h:float -> 
    color:Simple_color.emacs_color -> 
    unit -> unit

  method fill_text_scaled:
    ?rotate:float ->
    x:float -> y:float -> 
    size:float -> string -> unit

  method fill_text_scaled_return_width:
    ?rotate:float ->
    x:float -> y:float -> 
    size:float -> string -> 
    float

  method text_extents_scaled:
    string -> size:float -> float * float

  method user_to_device_font_size:
    float -> float

end
