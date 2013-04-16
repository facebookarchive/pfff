(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2013 Facebook
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
module T = Treemap

module M = Model_codemap

(*
module Flag = Flag_visual
open Model2
module Controller = Controller2
module Style = Style2
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The overlays *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* The current filename *)
(* ---------------------------------------------------------------------- *)

let draw_label_overlay ~cr_overlay ~dw ~x ~y r =

  raise Todo
(*
  let txt = r.T.tr_label in

  let readable_txt = 
    if dw.root = txt (* when we are fully zoomed on one file *)
    then "root"
    else 
      Common.filename_without_leading_path dw.root txt in

  let readable_txt =
    if String.length readable_txt > 25
    then 
      let dirs = Filename.dirname readable_txt +> Common.split "/" in
      let file = Filename.basename readable_txt in
      spf "%s/.../%s" (List.hd dirs) file
    else readable_txt
  in

  Cairo.select_font_face cr_overlay "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size cr_overlay Style2.font_size_filename_cursor;
      
  let extent = CairoH.text_extents cr_overlay readable_txt in
  let tw = extent.Cairo.text_width in
  let th = extent.Cairo.text_height in

  let refx = x - tw / 2. in
  let refy = y in

  CairoH.fill_rectangle ~cr:cr_overlay 
    ~x:(refx + extent.Cairo.x_bearing) ~y:(refy + extent.Cairo.y_bearing)
    ~w:tw ~h:(th * 1.2)
    ~color:"black"
    ~alpha:0.5
    ();

  Cairo.move_to cr_overlay refx refy;
  Cairo.set_source_rgba cr_overlay 1. 1. 1.    1.0;
  CairoH.show_text cr_overlay readable_txt;
  
  (*
  Cairo.set_source_rgb cr_overlay 0.3 0.3 0.3;
  Cairo.move_to cr_overlay x y;
  Cairo.line_to cr_overlay (x + 10.) (y + 10.);
  Cairo.stroke cr_overlay;
  *)
  ()
*)

(* ---------------------------------------------------------------------- *)
(* The current rectangles *)
(* ---------------------------------------------------------------------- *)

(*
let draw_rectangle_overlay ~cr_overlay ~dw (r, middle, r_englobing) =
  Cairo.save cr_overlay;
  View_mainmap.zoom_pan_scale_map cr_overlay dw;
  CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"white" r.T.tr_rect;

  CairoH.draw_rectangle_figure
    ~cr:cr_overlay ~color:"blue" r_englobing.T.tr_rect;
  Draw_labels.draw_treemap_rectangle_label_maybe 
    ~cr:cr_overlay ~color:(Some "red") ~zoom:dw.zoom r_englobing;

  middle +> Common.index_list_1 +> List.iter (fun (r, i) ->
    let color = 
      match i with
      | 1 -> "grey70"
      | 2 -> "grey40"
      | _ -> spf "grey%d" (max 1 (50 -.. (i *.. 10)))
    in
    CairoH.draw_rectangle_figure
      ~cr:cr_overlay ~color r.T.tr_rect;
    Draw_labels.draw_treemap_rectangle_label_maybe 
      ~cr:cr_overlay ~color:(Some color) ~zoom:dw.zoom r;
  );
    
  Cairo.restore cr_overlay;
  ()
*)

(* ---------------------------------------------------------------------- *)
(* The selected rectangles *)
(* ---------------------------------------------------------------------- *)

(*
let draw_searched_rectangles ~cr_overlay ~dw =
  Cairo.save cr_overlay;
  View_mainmap.zoom_pan_scale_map cr_overlay dw;

  dw.current_searched_rectangles +> List.iter (fun r ->
    CairoH.draw_rectangle_figure ~cr:cr_overlay 
      ~color:"yellow" r.T.tr_rect
  );
  (* 
   * would also like to draw not matching rectangles
   * bug the following code is too slow on huge treemaps. 
   * Probably because it is doing lots of drawing and alpha
   * computation.
   *
   * old:
   * let color = Some "grey3" in
   * Draw.draw_treemap_rectangle ~cr:cr_overlay 
   * ~color ~alpha:0.3
   * r
   *)
  Cairo.restore cr_overlay;
  ()
*)

(*****************************************************************************)
(* Assembling overlays *)
(*****************************************************************************)

(* was called motion_notify_refresher in gtk version *)

let mousemove
 (ctx: Canvas_helpers.context) (w: Model_codemap.world_client) ev =
  let device_x, device_y = ev##clientX, ev##clientY in
  pr2 (spf "mousemove device coord: %d x %d" device_x device_y);

  (* clear overlay *)
  ctx#canvas_ctx##clearRect (0., 0., T.xy_ratio, 1.0);

  let (x, y) = ctx#device_to_user ~x:device_x ~y:device_y in
  pr2 (spf "motion user coord: %f, %f" x y);

(*
  let r_opt = M.find_rectangle_at_user_point dw user in
  r_opt +> Common.do_option (fun (r, middle, r_englobing) ->
    let txt = r.T.tr_label in
    !Controller._statusbar_addtext txt;
    
    draw_label_overlay ~cr_overlay ~dw ~x ~y r;
    draw_rectangle_overlay ~cr_overlay ~dw (r, middle, r_englobing);
    
    if dw.dw_settings.draw_searched_rectangles;
    then
        draw_searched_rectangles ~cr_overlay ~dw;
    
    Controller.current_r := Some r;
    
    (* it has been computed, use it then *)
    if Hashtbl.mem _hmemo_surface (r.T.tr_label, dw.zoom) &&
      dw.in_zoom_incruste
    then
      draw_zoomed_overlay ~cr_overlay ~user ~dw ~x ~y r;
      
  );
*)
