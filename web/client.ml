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
let width = 1200
let height = 750

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
