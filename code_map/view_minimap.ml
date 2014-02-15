(*s: view_minimap.ml *)
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

module GR = Gdk.Rectangle
module F = Figures
module T = Treemap
module CairoH = Cairo_helpers
module Flag = Flag_visual
module Controller = Controller2

(*****************************************************************************)
(* Scaling *)
(*****************************************************************************)

(*s: scale_minimap *)
(*
let scale_minimap cr dw =
  (* no zoom, no pan, no clippnig *)
  Cairo.translate cr 0.0 0.0;
  Cairo.scale cr 
    (1.0 * (float_of_int dw.width_minimap / T.xy_ratio))
    (1.0 * (float_of_int dw.height_minimap))
*)
(*e: scale_minimap *)

(*s: with_minimap *)
(*
let with_minimap dw f =
  let cr = Cairo_lablgtk.create dw.pm_minimap#pixmap in
  scale_minimap cr dw;
  f cr
*)
(*e: with_minimap *)

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(*s: paint_minimap *)
(*
let paint_minimap2 dw = 
  let cr = Cairo_lablgtk.create dw.pm_minimap#pixmap in
  dw.pm_minimap#rectangle 
    ~x:0 ~y:0 
    ~width:dw.width_minimap ~height:dw.height_minimap
    ~filled:true () ;

  scale_minimap cr dw;

  let rects = dw.treemap in
  (* draw the rectangles *)
  rects +> List.iter (Draw_macrolevel.draw_treemap_rectangle ~cr);

  (* draw the labels, if have enough space *)
  rects +> List.iter 
    (Draw_labels.draw_treemap_rectangle_label_maybe ~cr ~zoom:1.0 ~color:None);

  (* draw the zoom rectangle *)
  let user_rect = 
    (* device_to_user_area dw  *)
     raise Todo
  in
  CairoH.draw_rectangle_figure ~cr ~color:"white" user_rect;
  ()
*)

(*
let paint_minimap dw = 
  Common.profile_code "View.paint minimap" (fun () -> paint_minimap2 dw)
*)
(*e: paint_minimap *)

(* ---------------------------------------------------------------------- *)
(* The mini-map *)
(* ---------------------------------------------------------------------- *)

(*s: expose_minimap *)
(*
let expose_minimap da dw_ref ev = 
  let dw = !dw_ref in

  (* todo? opti? don't paint if not needed ? *)
  (*paint_minimap dw;*)

  let area = GdkEvent.Expose.area ev in
  let x = GR.x area in
  let y = GR.y area in
  let width = GR.width area in
  let height = GR.height area in

  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in

  Common.profile_code "View.put_pixmap mini" (fun () ->
    d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height 
      dw.pm_minimap#pixmap;
  );
  true
*)
(*e: expose_minimap *)

(*s: configure_minimap *)
(*
let configure_minimap da2 dw_ref ev = 
  let dw = !dw_ref in

  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in
  dw.width_minimap <- w;
  dw.height_minimap <- h;
  dw.pm_minimap <- new_pixmap dw.width_minimap dw.height_minimap;
  true
*)
(*e: configure_minimap *)

(* ---------------------------------------------------------------------- *)
(* The mini-map *)
(* ---------------------------------------------------------------------- *)

(*s: motion_notify_minimap *)
(*
let motion_notify_minimap (da, da2) dw ev =
  let dw = !dw in

  let x = GdkEvent.Motion.x ev in
  let y = GdkEvent.Motion.y ev in
  pr2 ("motion minimap");

  if dw.in_dragging then begin

    let deltax = x -. dw.drag_pt_minimap.Cairo.x in
    let deltay = y -. dw.drag_pt_minimap.Cairo.y in

    let deltax_user = 
      with_minimap dw (fun cr -> CairoH.device_to_user_distance_x cr deltax)
    in
    let deltay_user = 
      with_minimap dw (fun cr -> CairoH.device_to_user_distance_y cr deltay)
    in
    dw.xtrans <- dw.xtrans -. deltax_user;
    dw.ytrans <- dw.ytrans -. deltay_user;
    
  
    (* pr2_gen (deltax, deltay); *)
    dw.drag_pt_minimap <- { Cairo.x = x ; Cairo.y = y } ;
    
    (* TODO: opti, should not recompute when just move! *)
(* TOREPUT
    paint dw;
*)

    GtkBase.Widget.queue_draw da#as_widget;
    GtkBase.Widget.queue_draw da2#as_widget;
    
    true
  end else begin
    true
  end
*)
(*e: motion_notify_minimap *)

(*s: button_action_minimap *)
(*
let button_action_minimap (da,da2) dw ev =
  let dw = !dw in

  match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->
      pr2 ("button pressed minimap");

      dw.drag_pt_minimap <- { 
        Cairo.x = GdkEvent.Button.x ev ; 
        Cairo.y = GdkEvent.Button.y ev 
      };
      dw.in_dragging <- true;

      true

  | `BUTTON_RELEASE ->
      pr2 ("button released minimap");
      dw.in_dragging <- false;

      (* TODO: opti *)
(* TOREPUT
      paint dw;
*)

      GtkBase.Widget.queue_draw da#as_widget;
      GtkBase.Widget.queue_draw da2#as_widget;
      true
  | _ -> false
*)
(*e: button_action_minimap *)



(*e: view_minimap.ml *)
