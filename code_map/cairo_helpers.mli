(*s: cairo_helpers.mli *)

(*s: cairo helpers functions sig *)
val fill_rectangle:
  ?alpha:float ->
  cr:Cairo.t -> 
  x:float -> y:float -> w:float -> h:float ->
  color:Simple_color.emacs_color -> 
  unit ->
  unit

val draw_rectangle_figure:
  cr:Cairo.t -> 
  color:Simple_color.emacs_color -> 
  Figures.rectangle -> unit

val draw_rectangle_bis:
  cr:Cairo.t -> 
  color:Simple_color.color -> 
  line_width:float ->
  Figures.rectangle -> unit


val prepare_string : string -> string
val origin : Cairo.point

val device_to_user_distance_x : Cairo.t -> float -> float
val device_to_user_distance_y : Cairo.t -> float -> float
val user_to_device_distance_x : Cairo.t -> float -> float
val user_to_device_distance_y : Cairo.t -> float -> float

val device_to_user_size : Cairo.t -> float -> float
val user_to_device_font_size : Cairo.t -> float -> float
val cairo_point_to_point : Cairo.point -> Figures.point

val show_text : Cairo.t -> string -> unit
val text_extents : Cairo.t -> string -> Cairo.text_extents
val set_font_size: Cairo.t -> float -> unit

val clear : Cairo.t -> unit

val surface_of_pixmap :
  < pixmap : [> `drawable ] Gobject.obj; .. > -> [ `Any ] Cairo.surface

val distance_points : Cairo.point -> Cairo.point -> float

val is_old_cairo : unit -> bool
(*e: cairo helpers functions sig *)
(*e: cairo_helpers.mli *)
