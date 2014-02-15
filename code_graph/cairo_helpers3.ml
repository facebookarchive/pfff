(* Yoann Padioleau
 * 
 * Copyright (C) 2012 Facebook
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
open Common2
open Common
(* floats are the norm in graphics *)
open Common2.ArithFloatInfix

open Figures
module F = Figures
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* todo: factorize with code_map/cairo_helpers.ml *)

(*****************************************************************************)
(* Text related *)
(*****************************************************************************)

(* May have to move this in commons/ at some point *)

let re_space = Str.regexp "^[ ]+$"

(* !does side effect on the (mutable) string! *)
let prepare_string s = 
  if s ==~ re_space
  then 
    s ^ s (* double it *)
  else begin
    for i = 0 to String.length s -.. 1 do
      let c = String.get s i in
      if int_of_char c >= 128
      then String.set s i 'Z'
      else 
        if c = '\t'
        then String.set s i ' '
      else ()
    done;
    s
  end


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

let text_extents cr s = 
  Common.profile_code "CairoH.cairo_text_extent" (fun () -> 
    (*if s = ""  then fake_text_extents else *)
    Cairo.text_extents cr s
  )

let set_font_size cr font_size =
  Common.profile_code "CairoH.set_font_size" (fun () ->
    Cairo.set_font_size cr font_size
  )

(*****************************************************************************)
(* Distance conversion *)
(*****************************************************************************)
let origin = { Cairo. x = 0.; y = 0. }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)
let clear cr =
  Cairo.set_source_rgba cr 0. 0. 0.   0.;
  Cairo.set_operator cr Cairo.OPERATOR_SOURCE;
  Cairo.paint cr;
  Cairo.set_operator cr Cairo.OPERATOR_OVER;
  ()

let set_source_color ?(alpha=1.) ~cr ~color () = 
  (let (r,g,b) = color +> Color.rgbf_of_string in
  Cairo.set_source_rgba cr r g b alpha;
  )

let fill_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.fill cr;
  ()

let fill_rectangle ?alpha ~cr ~color r = 
  set_source_color ?alpha ~cr ~color ();

  Cairo.move_to cr r.p.x r.p.y;
  Cairo.line_to cr r.q.x r.p.y;
  Cairo.line_to cr r.q.x r.q.y;
  Cairo.line_to cr r.p.x r.q.y;
  Cairo.line_to cr r.p.x r.p.y;
  Cairo.fill cr;
    ()

let draw_rectangle ?alpha ~cr ~color ~line_width r =
  set_source_color ?alpha ~cr ~color ();
  Cairo.set_line_width cr line_width;

  Cairo.move_to cr r.p.x r.p.y;
  Cairo.line_to cr r.q.x r.p.y;
  Cairo.line_to cr r.q.x r.q.y;
  Cairo.line_to cr r.p.x r.q.y;
  Cairo.line_to cr r.p.x r.p.y;
  Cairo.stroke cr;
  ()

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
