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
(* Helpers *)
(*****************************************************************************)
let readable ~root str =
  String.sub str (String.length root) 
    (String.length str -.. String.length root)

(*****************************************************************************)
(* The overlays *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* The current filename *)
(* ---------------------------------------------------------------------- *)

let draw_label_overlay (ctx: Canvas_helpers.context) w ~x ~y r =
  let txt = r.T.tr_label in
  (* we do some opti in server_codemap now which can lead to empty txt *)
  if txt <> "" then begin

  let readable_txt = 
    if w.M.root = txt (* when we are fully zoomed on one file *)
    then "root"
    else readable ~root:w.M.root txt
  in
(*
  let readable_txt =
    if String.length readable_txt > 25
    then 
      let dirs = Filename.dirname readable_txt +> Common.split "/" in
      let file = Filename.basename readable_txt in
      spf "%s/.../%s" (List.hd dirs) file
    else readable_txt
  in
*)
  let _, font_size = 
    ctx#device_to_user 
      ~x:0
      ~y:Style_codemap.font_size_filename_cursor_device_world
  in
      
  let tw, th = ctx#text_extents_scaled readable_txt ~size:font_size in

  let refx = x - tw / 2. in
  let refy = y in
  let x_bearing = 0. in
  let y_bearing = 0. - th in

  ctx#fill_rectangle_xywh
    ~x:(refx + x_bearing) ~y:(refy + y_bearing)
    ~w:tw ~h:(th * 1.4)
    ~color:"black"
    ~alpha:0.5 ();

  ctx#fillStyle "white";
  ctx#fill_text_scaled readable_txt ~size:font_size ~x:refx ~y:refy;
  end 

(* ---------------------------------------------------------------------- *)
(* The current rectangles *)
(* ---------------------------------------------------------------------- *)

let draw_rectangle_overlay 
  (ctx: Canvas_helpers.context) w (r, middle, r_englobing) =

  let line_width = ctx#device_to_user_size 3 in

  ctx#draw_rectangle ~color:"white" r.T.tr_rect ~line_width;
  ctx#draw_rectangle ~color:"blue" r_englobing.T.tr_rect ~line_width;
  Draw_labels.draw_treemap_rectangle_label_maybe 
    ctx ~color:(Some "red") r_englobing;

  middle +> Common.index_list_1 +> List.iter (fun (r, i) ->
    let color = 
      match i with
      | 1 -> "grey70"
      | 2 -> "grey40"
      | _ -> spf "grey%d" (max 1 (50 -.. (i *.. 10)))
    in
    ctx#draw_rectangle ~color r.T.tr_rect ~line_width;
    Draw_labels.draw_treemap_rectangle_label_maybe 
      ctx ~color:(Some color) r;
  )


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
 (ctx: Canvas_helpers.context) (w: Model_codemap.world_client) 
 (elt: Dom_html.element Js.t) ev =
  let device_x, device_y = Common_client.get_position elt ev in
  pr2 (spf "mousemove device coord: %d x %d" device_x device_y);

  (* clear overlay *)
  ctx#canvas_ctx##clearRect (0., 0., T.xy_ratio, 1.0);

  let (x, y) = ctx#device_to_user ~x:device_x ~y:device_y in
  pr2 (spf "motion user coord: %f, %f" x y);
  let user = { Figures.x = x; y = y } in

  let r_opt = M.find_rectangle_at_user_point w user in
  r_opt +> Common.do_option (fun (r, middle, r_englobing) ->
    let txt = r.T.tr_label in
    (* !Controller._statusbar_addtext txt; *)

    pr2 (spf "found: %s" txt);
    draw_label_overlay ctx w ~x ~y r;
    draw_rectangle_overlay ctx w (r, middle, r_englobing);
    
(*
    if dw.dw_settings.draw_searched_rectangles;
    then
        draw_searched_rectangles ~cr_overlay ~dw;
    
    Controller.current_r := Some r;
    
    (* it has been computed, use it then *)
    if Hashtbl.mem _hmemo_surface (r.T.tr_label, dw.zoom) &&
      dw.in_zoom_incruste
    then
      draw_zoomed_overlay ~cr_overlay ~user ~dw ~x ~y r;
*)
  )
