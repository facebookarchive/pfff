(*s: style2.mli *)

val windows_params : int -> int * int * int * int

val size_font_multiplier_of_categ :
  font_size_real:float -> Highlight_code.category option -> float

val threshold_draw_dark_background_font_size_real : float

val zoom_factor_incruste_mode : float

val font_size_filename_cursor: float

val font_text: string

(*e: style2.mli *)
