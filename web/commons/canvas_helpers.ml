(* Yoann Padioleau
 * 
 * Copyright (C) 2011, 2013 Facebook
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
open Common2.ArithFloatInfix

open Figures
module F = Figures
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type css_color = string

class context ~ctx = object
end

(*****************************************************************************)
(* Color *)
(*****************************************************************************)
let rgba_of_rgbf (r,g,b) alpha =
  let f_to_i f = int_of_float (100. * f) in
  let (r, g, b) = f_to_i r, f_to_i g, f_to_i b in
  spf "rgba(%d%%, %d%%, %d%%, %f)" r g b alpha

(*****************************************************************************)
(* Figures *)
(*****************************************************************************)
let draw_line ctx (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- size;
  ctx##beginPath();
  ctx##moveTo(x1, y1);
  ctx##lineTo(x2, y2);
  ctx##stroke()

(*****************************************************************************)
(* Text *)
(*****************************************************************************)

(*****************************************************************************)
(* Distance conversion *)
(*****************************************************************************)
