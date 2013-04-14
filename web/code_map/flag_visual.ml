
let verbose_visual_server = ref false

let boost_label_size = ref false

let threshold_draw_label_font_size_real = ref 
  10.

let threshold_draw_content_font_size_real = ref 
  0.6

(* big and auto-generated files can take too much time to render *)
let threshold_draw_content_nblines = 
  ref 25000.


let threshold_draw_dark_background_font_size_real = 1.
