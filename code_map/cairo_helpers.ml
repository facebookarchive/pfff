(*s: cairo_helpers.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: Facebook copyright *)
open Common
(* floats are the norm in graphics *)
open Common2.ArithFloatInfix

open Figures
module F = Figures
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let (==~) = Common2.(==~)

(*****************************************************************************)
(* Text related *)
(*****************************************************************************)

(* May have to move this in commons/ at some point *)

let re_space = Str.regexp "^[ ]+$"
let re_tab = Str.regexp "^[\t]+$"

(*s: cairo helpers functions *)
(* !does side effect on the (mutable) string! *)
let prepare_string s = 
  match s with
  | _ when s ==~ re_space -> s ^ s (* double it *)
  | _ when s ==~ re_tab -> 
      Str.global_replace (Str.regexp "\t") "        " s
  | _ ->
    for i = 0 to String.length s -.. 1 do
      let c = String.get s i in
      if int_of_char c >= 128
      then String.set s i 'Z'
      else 
        (* still useful now that have re_tab case above? *)
        if c = '\t'
        then String.set s i ' '
      else ()
    done;
    s

let show_text2 cr s =
  (* this 'if' is only for compatibility with old versions of cairo
   * that returns some out_of_memory error when applied to empty strings
   *)
  if s = "" then () else 
  try 
    let s' = prepare_string s in
    Cairo.show_text cr s'
  with _exn ->
    let status = Cairo.status cr in
    let s2 = Cairo.string_of_status status in
    failwith ("Cairo pb: " ^ s2 ^ " s = " ^ s)

let show_text a b = 
  Common.profile_code "View.cairo_show_text" (fun () -> show_text2 a b)

(*
let fake_text_extents = 
  { Cairo.
    x_bearing   = 0.1; y_bearing   = 0.1;
    text_width  = 0.1; text_height = 0.1;
    x_advance   = 0.1; y_advance   = 0.1 ;
  }
*)

let text_extents cr s = 
  Common.profile_code "CairoH.cairo_text_extent" (fun () -> 
    (*if s = ""  then fake_text_extents else *)
    Cairo.text_extents cr s
  )

(* just wrap it here so that we can profile it *)
let set_font_size cr font_size =
  Common.profile_code "CairoH.set_font_size" (fun () ->
    Cairo.set_font_size cr font_size
  )

(*****************************************************************************)
(* Distance conversion *)
(*****************************************************************************)

let origin = { Cairo. x = 0.; y = 0. }

let device_to_user_distance_x cr deltax = 
  let pt = Cairo.device_to_user_distance cr { origin with Cairo.x = deltax } in
  pt.Cairo.x
let device_to_user_distance_y cr deltay = 
  let pt = Cairo.device_to_user_distance cr { origin with Cairo.y = deltay } in
  pt.Cairo.y

let user_to_device_distance_x cr deltax = 
  let pt = Cairo.user_to_device_distance cr { origin with Cairo.x = deltax } in
  pt.Cairo.x
let user_to_device_distance_y cr deltay = 
  let pt = Cairo.user_to_device_distance cr { origin with Cairo.y = deltay } in
  pt.Cairo.y

(* TODO: this is buggy, as we can move the map which can led to
 * some device_to_user to translate to x = 0
 *)
let device_to_user_size cr size = 
  let device = { Cairo.x = size; Cairo.y = 0.; } in
  let user = Cairo.device_to_user cr device in
  user.Cairo.x

(* still needed ? can just call device_to_user_size ? *)
let user_to_device_font_size cr font_size = 
  let user_dist = { Cairo.x = font_size; Cairo.y = font_size } in
  let device_dist = Cairo.user_to_device_distance cr user_dist in
  device_dist.Cairo.x

let cairo_point_to_point p = 
  { F.x = p.Cairo.x;
    F.y = p.Cairo.y;
  }

let distance_points p1 p2 =
  abs_float (p2.Cairo.x - p1.Cairo.x) + 
  abs_float (p2.Cairo.y - p1.Cairo.y)

(*****************************************************************************)
(* Surface *)
(*****************************************************************************)

(* see http://cairographics.org/FAQ/#clear_a_surface *)
let clear cr =
  Cairo.set_source_rgba cr 0. 0. 0.   0.;
  Cairo.set_operator cr Cairo.OPERATOR_SOURCE;
  Cairo.paint cr;
  Cairo.set_operator cr Cairo.OPERATOR_OVER;
  ()

let surface_of_pixmap pm =
  let cr = Cairo_lablgtk.create pm#pixmap in
  Cairo.get_target cr

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let fill_rectangle ?(alpha=1.) ~cr ~x ~y ~w ~h ~color () = 
  (let (r,g,b) = color +> Color.rgbf_of_string in
  Cairo.set_source_rgba cr r g b alpha;
  );
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.fill cr;
  ()

let draw_rectangle_figure ~cr ~color r =
  (let (r,g,b) = color +> Color.rgbf_of_string in
  Cairo.set_source_rgb cr r g b;
  );
 let line_width = device_to_user_size cr 3. in

  Cairo.set_line_width cr line_width; (* ((r.q.y - r.p.y) / 30.); *)

  Cairo.move_to cr r.p.x r.p.y;
  Cairo.line_to cr r.q.x r.p.y;
  Cairo.line_to cr r.q.x r.q.y;
  Cairo.line_to cr r.p.x r.q.y;
  Cairo.line_to cr r.p.x r.p.y;
  Cairo.stroke cr;
  ()

(* factorize with draw_rectangle. don't use buggy device_to_user_size !!!
*)
let draw_rectangle_bis ~cr ~color ~line_width r =
  (let (r,g,b) = 
    color +> Color.rgb_of_color +> Color.rgbf_of_rgb
    in
   Cairo.set_source_rgb cr r g b;
  );
  Cairo.set_line_width cr line_width;

  Cairo.move_to cr r.p.x r.p.y;
  Cairo.line_to cr r.q.x r.p.y;
  Cairo.line_to cr r.q.x r.q.y;
  Cairo.line_to cr r.p.x r.q.y;
  Cairo.line_to cr r.p.x r.p.y;
  Cairo.stroke cr;
  ()
(*e: cairo helpers functions *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let is_old_cairo () = 
  let s = Cairo.compile_time_version_string in
  match () with
  | _ when s =~ "1\\.[89]\\.*" -> false
  | _ -> true


(*e: cairo_helpers.ml *)
