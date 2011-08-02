(* Yoann Padioleau
 * 
 * Copyright (C) 2011 Facebook
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

module F = Figures
module Color = Simple_color

open Figures

(* floats are the norm in graphics *)
open Common.ArithFloatInfix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type context = Dom_html.canvasRenderingContext2D Js.t

(*****************************************************************************)
(* Color *)
(*****************************************************************************)
let rgba_of_color (r,g,b) alpha =
  raise Todo

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
