(*s: draw_macrolevel.ml *)
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
open Common2.ArithFloatInfix

open Figures (* for the fields *)
module T = Treemap
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Drawing a treemap rectangle *)
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
(* Layers macrolevel *)
(*****************************************************************************)

(* How should we draw layer information at the macro level ?
 *   
 *  - fill the rectangle with the color of one layer ? 
 *  - separate equally among layers ? 
 *  - draw on top of the existing archi color ?
 *  - draw circles instead of rectangle so have quantitative information too 
 *    (like I was doing when display git related commit information).
 * 
 * It is maybe good to not draw on top of the existing archi_code color. 
 * Too many colors kill colors. Also we can not convey quantitative 
 * information by coloring with full rectangles (instead of the random
 * circles trick) but for some layers like security it is probably better.
 * Don't care so much about how many bad calls; care really about
 * number of files with bad calls in them.
 * 
 * So for now we just fill rectangles with colors from the layer and
 * when a file matches multiple layers we split the rectangle in equal
 * parts.
 *)

let draw_trect_using_layers ~cr layers_with_index rect =
  (* don't use archi_code color. Just black and white *)
  let is_file = not rect.T.tr_is_node in
  let color = if is_file then "white" else "black" in
  draw_treemap_rectangle ~cr ~color:(Some color) rect;

  if is_file then begin
    let file = rect.T.tr_label in
    
    let color_info = 
      try Hashtbl.find layers_with_index.Layer_code.macro_index file
      with Not_found -> []
    in
    (* What to draw? TODO a splitted rectangle? *)
    let sorted = Common2.sort_by_key_highfirst color_info in
    (match sorted with
    | [] -> ()
    | (_float, color)::_rest ->
      draw_treemap_rectangle ~cr ~color:(Some color) rect;
    );
  end

(*s: draw_summary_content *)
(*e: draw_summary_content *)
(*e: draw_macrolevel.ml *)
