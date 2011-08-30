open Common
open Common.ArithFloatInfix
open Common_client

module T = Treemap
module F = Figures
module Color = Simple_color
module CanvasH = Canvas_helpers

open Figures (* for the fields *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)

(* see visual/style2.ml *)
let width = 2350 (* 1200 *)
let height = 1400 (* 750 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)
let scale_zoom_pan_map ~width ~height ctx =
  (* original matrix *)
  ctx##setTransform (1.,0.,0.,1.,0.,0.);

  ctx##scale (float_of_int width /. T.xy_ratio,
              float_of_int height);
  ()

let draw_treemap_rendering (rects: Treemap.treemap_rendering) =
  pr2 "draw_treemap_rendering";
  pr2 (spf "# rects = %d " (List.length rects));

  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- width; 
  canvas##height <- height;

  scale_zoom_pan_map ~width ~height ctx;

  rects +> List.iter (fun rect -> 
    Draw_macrolevel.draw_treemap_rectangle ctx rect
  );

  Dom.appendChild Dom_html.document##body canvas;
  ()


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
