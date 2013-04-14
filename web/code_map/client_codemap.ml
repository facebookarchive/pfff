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

  pr2 "paint";
  pr2 (spf "# rects = %d " (List.length w.rects));

  let rects = w.rects in

  (* phase 1, draw the rectangles *)
  rects +> List.iter (Draw_macrolevel.draw_treemap_rectangle ctx);
  (* TODO: handle layers *)

  (* phase 2, draw the labels, if have enough space *)
  rects +> List.iter (Draw_labels.draw_treemap_rectangle_label_maybe 
                        ctx ~color:None);

  (* phase 3, draw the content, if have enough space *)
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
