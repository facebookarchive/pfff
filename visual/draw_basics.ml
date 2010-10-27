(*s: draw_basics.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
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

(* 
 * Floats are the norm in graphics. 
 * note: with ocaml 3.12 could also use the Float.(...) local open extension
 *)
open Common.ArithFloatInfix

open Figures (* for the fields *)
open Model2 (* for the fields *)

module T = Treemap

module Color = Simple_color

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Basics *)
(*****************************************************************************)

(*s: draw_treemap_rectangle() *)
let draw_treemap_rectangle2 ~cr ?(color=None) ?(alpha=1.) rect =
  let r = rect.T.tr_rect in

  (let (r,g,b) = 
    let (r,g,b) = rect.T.tr_color +> Color.rgb_of_color +> Color.rgbf_of_rgb in
    match color with
    | None -> (r,g,b)
    | Some c -> 
        let (r2,g2,b2) = c +> Color.rgbf_of_string in
        (r2 + r / 20., g2 + g / 20., b2 + b / 20.)
  in
  Cairo.set_source_rgba cr r g b (alpha);
  );

  Cairo.move_to cr r.p.x r.p.y;
  Cairo.line_to cr r.q.x r.p.y;
  Cairo.line_to cr r.q.x r.q.y;
  Cairo.line_to cr r.p.x r.q.y;
  Cairo.fill cr;
  ()

let draw_treemap_rectangle ~cr ?color ?alpha a =
  Common.profile_code "View.draw_treemap_rectangle" (fun () -> 
    draw_treemap_rectangle2 ~cr ?color ?alpha a)
(*e: draw_treemap_rectangle() *)

(*****************************************************************************)
(* Color of entity *)
(*****************************************************************************)
(*e: draw_basics.ml *)
