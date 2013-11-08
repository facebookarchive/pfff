(*s: flag_visual.ml *)

let verbose_visual = ref false

(* was 0.4, but on linux the anti-aliasing seems to not be as good
 * as on mac (possibly because I have only an old cairo lib on my 
 * Linux machine
 *)
let threshold_draw_content_font_size_real = ref 
  0.9

(* big and auto-generated files can take too much time to render *)
let threshold_draw_content_nblines = 
  ref 25000.

let threshold_draw_label_font_size_real = ref 
  10.

let threshold_nb_rects_draw_content = ref 2500

let threshold_too_many_entities = ref 600000

let top_n = ref 100

let boost_label_size = ref false

let debug_gc = ref false

(* Ancient does not interact well with hashtbl and ocaml polymorphic
 * equality and hash. Have to use a functorized hashtbl which sucks.
 *)
let use_ancient = ref false

let disable_fonts = ref false

let extra_filter = ref (None: string option) (* regexp *)
(*e: flag_visual.ml *)
