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

open Figures (* for the fields *)
module F = Figures
module Color = Simple_color

module T = Treemap

open Model_codemap
module M = Model_codemap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize code with pfff/code_map/view_mainmap.ml *)

(*****************************************************************************)
(* Init *)
(*****************************************************************************)

(* copy paste of view_codegraph.ml *)
let init w =

  let canvas_elt = retrieve "main_canvas" in
  let canvas = canvas_elt +> Dom_html.CoerceTo.canvas +> unopt in

  let d = Dom_html.window##document in

  let canvas_paint = Dom_html.createCanvas d in
  canvas_paint##width <- w.M.width;
  canvas_paint##height <- w.M.height;
  let ctx_paint = new Canvas_helpers.context
    ~ctx:(canvas_paint##getContext (Dom_html._2d_))
    ~width:w.M.width
    ~height:w.M.height
    ~xy_ratio:T.xy_ratio
  in

  let canvas_overlay = Dom_html.createCanvas d in
  canvas_overlay##width <- w.M.width;
  canvas_overlay##height <- w.M.height;
  let ctx_overlay = new Canvas_helpers.context
    ~ctx:(canvas_overlay##getContext (Dom_html._2d_))
    ~width:w.M.width
    ~height:w.M.height
    ~xy_ratio:T.xy_ratio
  in

  let ctx_final = new Canvas_helpers.context 
    ~ctx:(canvas##getContext (Dom_html._2d_))
    ~width:w.M.width
    ~height:w.M.height
    ~xy_ratio:T.xy_ratio
  in
  let refresh_drawing_area () =
    ctx_final#canvas_ctx##save();
    ctx_paint#canvas_ctx##save();
    ctx_overlay#canvas_ctx##save();

    ctx_final#canvas_ctx##moveTo (0., 0.);
    ctx_final#canvas_ctx##setTransform (1.,0.,0.,1.,0.,0.);
    ctx_paint#canvas_ctx##moveTo (0., 0.);
    ctx_paint#canvas_ctx##setTransform (1.,0.,0.,1.,0.,0.);
    ctx_overlay#canvas_ctx##moveTo (0., 0.);
    ctx_overlay#canvas_ctx##setTransform (1.,0.,0.,1.,0.,0.);
    
    (* have to set the transform back to normal otherwise drawImage
     * does not work, weird a bit.
     *)
    ctx_final#canvas_ctx##drawImage_fromCanvas
      (canvas_paint, 0., 0.);
    ctx_final#canvas_ctx##drawImage_fromCanvas
      (canvas_overlay, 0., 0.);

    ctx_final#canvas_ctx##restore();
    ctx_paint#canvas_ctx##restore();
    ctx_overlay#canvas_ctx##restore();
    ()
  in
  (* to debug *)
(*
  Dom.appendChild d##body canvas_paint;
  Dom.appendChild d##body canvas_overlay;
*)

  (ctx_paint, ctx_overlay, ctx_final),
  refresh_drawing_area, 
  canvas_elt

(*****************************************************************************)
(* Painting entry point *)
(*****************************************************************************)


let paint w main_service =

  let 
  (ctx_paint, ctx_overlay, ctx_final),
  refresh_drawing_area,
  canvas_elt
   = init w in

  pr2 "paint";
  pr2 (spf "# rects = %d " (List.length w.rects));

  let rects = w.rects in

  (* phase 1, draw the rectangles *)
  rects +> List.iter (Draw_macrolevel.draw_treemap_rectangle ctx_paint);
  (* TODO: handle layers *)

  (* phase 2, draw the labels, if have enough space *)
 
  Dom_html.window##setTimeout (Js.wrap_callback (fun () ->
  rects +> List.iter (Draw_labels.draw_treemap_rectangle_label_maybe 
                        ctx_paint ~color:None);
  ), 0.5) +> ignore;

  (* phase 3, draw the content, if have enough space *)
  (* TODO *)

  refresh_drawing_area ();

  canvas_elt##onmousemove <- Dom_html.handler (fun ev ->
    View_overlays_codemap.mousemove ctx_overlay w canvas_elt ev;
    refresh_drawing_area ();
    Js._false
  );

  canvas_elt##ondblclick <- Dom_html.handler (fun ev ->
    Lwt.async (fun () ->
      Interaction_codemap.mouseclick ctx_overlay w 
        main_service
        canvas_elt ev 
    );
    Js._false
  );

  ()



let test_paint_micro w fileinfo =
  let canvas =
    retrieve "main_canvas" +> 
      Dom_html.CoerceTo.canvas +>
      unopt
  in
  let canvas_ctx = canvas##getContext (Dom_html._2d_) in
  let ctx = new Canvas_helpers.context 
    ~ctx:canvas_ctx
    ~width:w.width
    ~height:w.height
    ~xy_ratio:T.xy_ratio
  in

  let r = {
    p = { x = 0.; y = 0.};
    q = { x = T.xy_ratio; y = 1. };
  }
  in
  let rect = { Treemap.
    tr_rect = r;
    tr_color = 1;
    tr_label = "TODO";
    tr_depth = 1;
    tr_is_node = false;
  }
  in
  Draw_microlevel.draw_treemap_rectangle_content_maybe ctx fileinfo rect;
  ()
