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

module M = Model_codegraph

(*****************************************************************************)
(* Painting entry point *)
(*****************************************************************************)

(* paint() creates the cairo context and adjusts the scaling if needed
 * and then calls the 'draw' functions.
 *)
let paint w =

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
    ~xy_ratio:M.xy_ratio
  in

  let canvas_overlay = Dom_html.createCanvas d in
  canvas_overlay##width <- w.M.width;
  canvas_overlay##height <- w.M.height;
  let ctx_overlay = new Canvas_helpers.context
    ~ctx:(canvas_overlay##getContext (Dom_html._2d_))
    ~width:w.M.width
    ~height:w.M.height
    ~xy_ratio:M.xy_ratio
  in

  let ctx_final = new Canvas_helpers.context 
    ~ctx:(canvas##getContext (Dom_html._2d_))
    ~width:w.M.width
    ~height:w.M.height
    ~xy_ratio:M.xy_ratio
  in
  let refresh_drawing_area () =
    ctx_final#canvas_ctx##drawImage_fromCanvas
      (canvas_paint, 0., 0.);
    ()
  in
  
  View_matrix_codegraph.draw_matrix ctx_paint w;
  refresh_drawing_area ();
(*
  canvas_elt##onmousemove <- Dom_html.handler (fun ev ->
    View_overlays_codegraph.mousemove ctx_overlay w ev;
    refresh_drawing_area ();
    Js._false
  )
*)
  ()
