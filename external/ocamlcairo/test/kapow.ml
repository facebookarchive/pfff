(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

let filename = "kapow.png"
let fontname = "Sans"
let default_text = "KAPOW"

let width = 384.
let height = 256.

let spikes = 10
let shadow_offset = 10.

let x_fuzz = 16.
let y_fuzz = 16.

let x_outer_radius = width  /. 2. -. x_fuzz -. shadow_offset
let y_outer_radius = height /. 2. -. y_fuzz -. shadow_offset

let x_inner_radius = x_outer_radius *. 0.7
let y_inner_radius = y_outer_radius *. 0.7

let pi = 4. *. atan 1.

let make_star_path cr =
  Random.init 42 ;

  for i = 0 to spikes - 1 do
    let x = width /. 2. +. cos (pi *. float (2 * i) /. float spikes) *. x_inner_radius +.
	Random.float x_fuzz in
    let y = height /. 2. +. sin (pi *. float (2 * i) /. float spikes) *. y_inner_radius +.
	Random.float y_fuzz in
    if i = 0 then
      Cairo.move_to cr x y
    else
      Cairo.line_to cr x y ;

    let x = width /. 2. +. cos (pi *. float (2 * i + 1) /. float spikes) *. x_outer_radius +.
	Random.float x_fuzz in
    let y = height /. 2. +. sin (pi *. float (2 * i + 1) /. float spikes) *. y_outer_radius +.
	Random.float y_fuzz in
    Cairo.line_to cr x y
  done ;
  Cairo.close_path cr

let bend_it { Cairo.x = x ; Cairo.y = y } =
  let cx = width /. 2. in
  let cy = 500. in
  let angle = pi /. 2. -. (x -. cx) /. width in
  let t = 3. *. pi /. 4. -. angle +. 0.05 in
  let angle = 3. *. pi /. 4. -. t ** 1.8 in
  let radius = cy -. (height /. 2. +. (y -. height /. 2.) *. t *. 2.) in

  { Cairo.x = cx +. cos angle *. radius;
    Cairo.y = cy -. sin angle *. radius }

let make_text_path cr x y text =
  Cairo.move_to cr x y ;
  Cairo.text_path cr text ;
  ignore 
    (Cairo.fold_path_flat cr
       (fun first -> function
	 | `MOVE_TO p -> 
	     if first then Cairo.new_path cr ;
	     Cairo.move_to_point cr (bend_it p) ; false
	 | `LINE_TO p ->
	     Cairo.line_to_point cr (bend_it p) ; false
	 | `CLOSE ->
	     Cairo.close_path cr ; false)
       true)
    
let draw text =
  let cr = 
    Cairo.create 
      (Cairo.image_surface_create 
	 Cairo.FORMAT_ARGB32 
	 (int_of_float width) (int_of_float height))in
  Cairo.set_line_width cr 2. ;

  Cairo.save cr ; begin
    Cairo.translate cr shadow_offset shadow_offset ;
    make_star_path cr ;
    Cairo.set_source_rgba cr 0. 0. 0. 0.5 ;
    Cairo.fill cr end ;
  Cairo.restore cr ;

  make_star_path cr ;
  let pattern = Cairo.Pattern.create_radial 
      (width /. 2.) (height /. 2.) 10.
      (width /. 2.) (height /. 2.) 230. in
  Cairo.Pattern.add_color_stop_rgba pattern 0. 1. 1. 0.2 1. ;
  Cairo.Pattern.add_color_stop_rgba pattern 1. 1. 0. 0.  1. ;
  Cairo.set_source cr pattern ;
  Cairo.fill cr ;

  make_star_path cr ;
  Cairo.set_source_rgb cr 0. 0. 0. ;
  Cairo.stroke cr ;

  Cairo.select_font_face cr fontname Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD ;
  Cairo.set_font_size cr 50. ;
  let extents = Cairo.text_extents cr text in
  let x = width /. 2. -. (extents.Cairo.text_width /. 2. +. extents.Cairo.x_bearing) in
  let y = height /. 2. -. (extents.Cairo.text_height /. 2. +. extents.Cairo.y_bearing) in
  make_text_path cr x y text ;
  
  let pattern = Cairo.Pattern.create_linear 
      (width /. 2. -. 10.) (height /. 4.)
      (width /. 2. +. 10.) (3. *. height /. 4.) in
  Cairo.Pattern.add_color_stop_rgba pattern 0. 1. 1. 1.  1. ;
  Cairo.Pattern.add_color_stop_rgba pattern 1. 0. 0. 0.4 1. ;
  Cairo.set_source cr pattern ;
  Cairo.fill cr ;

  make_text_path cr x y text ;
  Cairo.set_source_rgb cr 0. 0. 0. ;
  Cairo.stroke cr ;

  Cairo_png.surface_write_to_file (Cairo.get_target cr) filename

let _ =
  draw
    (if Array.length Sys.argv > 1 then Sys.argv.(1) else default_text)
