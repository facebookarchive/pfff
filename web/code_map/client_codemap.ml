(* Yoann Padioleau
 * 
 * Copyright (C) 2013 Facebook
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
open Common_client

open Figures (* for the fields *)
module Color = Simple_color

module T = Treemap
module F = Figures

open Model_codemap
module M = Model_codemap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize code with pfff/code_map/view_mainmap.ml *)

(*****************************************************************************)
(* JS Helpers *)
(*****************************************************************************)

(* TODO: from jflo slides, factorize with pfff/web/code_graph *)
let unopt x =
  Js.Opt.get x (fun () -> raise Not_found)
let retrieve id =
  unopt (Dom_html.document##getElementById (Js.string id))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(*****************************************************************************)
(* Painting entry point *)
(*****************************************************************************)

let paint w =

  let rects = w.rects in

  pr2 "paint";
  pr2 (spf "# rects = %d " (List.length rects));

  let canvas = 
    retrieve "main_canvas" +> 
      Dom_html.CoerceTo.canvas +>
      unopt
  in
  let ctx = canvas##getContext (Dom_html._2d_) in

(* TODO: factorize with pfff/web/code_graph *)
  
  (* ugly hack because html5 canvas does not handle using float size for fonts
   * when printing text in a scaled context.
   *)
  ctx##font <- Js.string (spf "bold 12 px serif" );
  let text = "MM" in
  let metric = ctx##measureText (Js.string text) in
  let width_text_etalon_orig_coord = metric##width / 2.0 in
  pr2 (spf "width text orig coord = %f" width_text_etalon_orig_coord);

  let orig_coord_width = float_of_int w.width in
  let normalized_coord_width = T.xy_ratio in

  let width_text_etalon_normalized_coord = 
    (normalized_coord_width * width_text_etalon_orig_coord) /
      orig_coord_width
  in
  pr2 (spf "width text normalized coord = %f" 
         width_text_etalon_normalized_coord);


  ctx##setTransform (1.,0.,0.,1.,0.,0.);
  ctx##scale (
    (float_of_int w.width / T.xy_ratio),
    (float_of_int w.height));

  let w = { w with
    width_text_etalon_normalized_coord;
    orig_coord_width;
    orig_coord_height = float_of_int w.height;
  }
  in

  (* phase 1, draw the rectangles *)
  rects +> List.iter (Draw_macrolevel.draw_treemap_rectangle ctx);
  (* TODO: handle layers *)

  (* phase 2, draw the labels, if have enough space *)

  (* phase 3, draw the content, if have enough space *)
  ()

(*****************************************************************************)
(* Test micro *)
(*****************************************************************************)

let width = 100
let height = 100
let draw_file lines =
  pr2 "draw_file";
  pr2 (spf "# lines = %d " (List.length lines));

  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- width; 
  canvas##height <- height;

(* http://stackoverflow.com/questions/6278249/html5-canvas-font-size 
 * with 200 px or more, firefox goes back to the default font
 * (chrome is fine though).
 *)

  ctx##font <- Js.string (spf "%d px serif" 200);
  ctx##fillStyle <- Js.string "#000";

  let text = "foobar" in
  let metric = ctx##measureText (Js.string text) in
  let width_text = metric##width in
  pr2 (spf "width text = %f" width_text);

  ctx##fillText (Js.string "foobar", 100., 100.);

(*
  scale_zoom_pan_map ~width ~height ctx;
  ctx##font <- Js.string (spf "%d%%" 2000);

  ctx##fillText (Js.string "foobar", 0.5, 0.5);
*)  

(*
  ctx##rotate(0.5);
  ctx##fillText (Js.string "foobar", 200., 100.);
*)
  Dom.appendChild Dom_html.document##body canvas;
  ()
