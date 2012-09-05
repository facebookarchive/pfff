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
open Common
(* floats are the norm in graphics *)
open Common.ArithFloatInfix

open Figures
module F = Figures
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* todo: factorize with codemap/cairo_helpers.ml *)

(*****************************************************************************)
(* Text related *)
(*****************************************************************************)

(*****************************************************************************)
(* Distance conversion *)
(*****************************************************************************)
let origin = { Cairo. x = 0.; y = 0. }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)
let clear cr =
  Cairo.set_source_rgba cr 1. 1. 1.   1.;
  Cairo.set_operator cr Cairo.OPERATOR_SOURCE;
  Cairo.paint cr;
  Cairo.set_operator cr Cairo.OPERATOR_OVER;
  ()

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

let draw_rectangle ~cr ~color ~line_width r =
  (let (r,g,b) = color +> Color.rgbf_of_string in
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

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
