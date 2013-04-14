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
open Common2.ArithFloatInfix
open Common_client

module T = Treemap
module F = Figures
module Color = Simple_color
module CanvasH = Canvas_helpers

open Figures (* for the fields *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize with pfff/code_map/ *)

(*****************************************************************************)
(* Drawing a treemap rectangle *)
(*****************************************************************************)

let draw_treemap_rectangle 
    (ctx2: Canvas_helpers.context) ?(color=None) ?(alpha=1.) rect =
  let r = rect.T.tr_rect in
  let ctx = ctx2#canvas_ctx in

  (let (r,g,b) = 
    let (r,g,b) = rect.T.tr_color +> Color.rgb_of_color +> Color.rgbf_of_rgb in
    match color with
    | None -> (r,g,b)
    | Some c -> 
        let (r2,g2,b2) = c +> Color.rgbf_of_string in
        (r2 + r / 20., g2 + g / 20., b2 + b / 20.)
    in
   ctx##fillStyle <- Js.string (CanvasH.css_color_of_rgbf (r,g,b) alpha)
  );
  ctx##lineWidth <- 0.;

  ctx##beginPath();
  ctx##moveTo (r.p.x, r.p.y);
  ctx##lineTo (r.q.x, r.p.y);
  ctx##lineTo (r.q.x, r.q.y);
  ctx##lineTo (r.p.x, r.q.y);
  ctx##closePath();
  ctx##fill();
  ()
