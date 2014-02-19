(*s: view_mainmap.ml *)
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
(* floats are the norm in graphics *)
open Common2.ArithFloatInfix
open Common2

module CairoH = Cairo_helpers
module K = GdkKeysyms

module F = Figures
module T = Treemap

module Flag = Flag_visual
open Model2
module M = Model2
module Ctl = Controller2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module calls Draw_macrolevel and Draw_microlevel and assembles
 * the final "painting" of the code "main map". It is called mainly by
 * View2.configure and Ui_navigation.go_dirs_and_file.
 * 
 * Painting is not the last element in the "main map" rendering pipeline.
 * There is also View_overlay which is called mainly when the user
 * moves the mouse which triggers the View_overlay.motion_refresher
 * callback which just add overlays on top of the already drawn (and
 * computationaly expensive) painting done here. 
 *)

(*****************************************************************************)
(* Scaling *)
(*****************************************************************************)

(*s: zoom_pan_scale_map *)
let zoom_pan_scale_map cr dw =
  Cairo.scale cr 
    (float_of_int dw.width / T.xy_ratio)
    (float_of_int dw.height)
  ;
  (* I first scale and then translate as the xtrans are in user coordinates *)
  Cairo.translate cr 0.0 0.0;
  ()
(*e: zoom_pan_scale_map *)

(*s: with_map *)
let with_map dw f = 
  let cr = Cairo_lablgtk.create dw.pm#pixmap in
  zoom_pan_scale_map cr dw;
  f cr
(*e: with_map *)

(*s: device_to_user_area *)
(* still needed ? reuse helper functions above ? *)
let device_to_user_area dw = 
  with_map dw (fun cr ->

    let device_point = { Cairo. x = 0.0; y = 0.0 } in
    let user_point1 = Cairo.device_to_user cr device_point in
    let device_point = { Cairo.x = float_of_int dw.width; 
                         Cairo.y = float_of_int dw.height; } in
    let user_point2 = Cairo.device_to_user cr device_point in
    
    { F.p = CairoH.cairo_point_to_point user_point1;
      F.q = CairoH.cairo_point_to_point user_point2;
    }
  )
(*e: device_to_user_area *)


(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(*s: paint *)

let paint_content_maybe_rect ~user_rect dw rect =
  let cr = Cairo_lablgtk.create dw.pm#pixmap in
  zoom_pan_scale_map cr dw;

  let context = M.context_of_drawing dw in

  let pos_and_line_opt = 
    Draw_microlevel.draw_treemap_rectangle_content_maybe
      ~cr  ~clipping:user_rect  ~context rect in
  pos_and_line_opt +> Common.do_option (fun pos_and_line ->
    Hashtbl.replace dw.microlevel rect pos_and_line
  );

  (* have to redraw the label *)
  Draw_labels.draw_treemap_rectangle_label_maybe 
    ~cr ~zoom:1.0 ~color:None rect;

  ()

(* todo: deadlock:  M.locked (fun () ->  ) dw.M.model.M.m *)
let lazy_paint ~user_rect dw () =
  pr2 "Lazy Paint";
  let start = Unix.gettimeofday () in
  while Unix.gettimeofday () - start < 0.3 do
    match !Ctl.current_rects_to_draw with
    | [] -> ()
    | x::xs ->
        Ctl.current_rects_to_draw := xs;
        pr2 (spf "Drawing: %s" (x.T.tr_label));
        paint_content_maybe_rect ~user_rect dw x;
  done;
  !Ctl._refresh_da ();
  if !Ctl.current_rects_to_draw = []
  then false
  else true


let paint2 dw = 

  !Ctl.paint_content_maybe_refresher +> Common.do_option GMain.Idle.remove;
  Ctl.current_rects_to_draw := [];

  let cr = Cairo_lablgtk.create dw.pm#pixmap in
  dw.pm#rectangle 
    ~x:0 ~y:0 
    ~width:dw.width ~height:dw.height 
    ~filled:true () ;

  pr2 (spf "paint");
  let user_rect = device_to_user_area dw in
  pr2 (F.s_of_rectangle user_rect);

  zoom_pan_scale_map cr dw;

  let rects = dw.treemap in
  let nb_rects = dw.nb_rects in

  (if not (Layer_code.has_active_layers dw.layers)
  then
    (* phase 1, draw the rectangles *)
    rects +> List.iter (Draw_macrolevel.draw_treemap_rectangle ~cr)
  else 
    rects +> List.iter (Draw_macrolevel.draw_trect_using_layers ~cr dw.layers)
  );

  (* phase 2, draw the labels, if have enough space *)
  rects +> List.iter (Draw_labels.draw_treemap_rectangle_label_maybe 
                         ~cr ~zoom:1.0  ~color:None);

  (* phase 3, draw the content, if have enough space *)
  if nb_rects < !Flag.threshold_nb_rects_draw_content
    (* draw_content_maybe calls nblines which is quite expensive so
     * want to limit it *)
  then begin
    Ctl.current_rects_to_draw := rects;
    Ctl.paint_content_maybe_refresher := 
      Some (Gui.gmain_idle_add ~prio:3000 (lazy_paint ~user_rect dw));
  end;

  (* also clear the overlay *)
  let cr_overlay = Cairo.create dw.overlay in
  CairoH.clear cr_overlay;

  ()

let paint dw = 
  Common.profile_code "View.paint" (fun () -> paint2 dw)
(*e: paint *)

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

(*s: key_pressed *)
(*
let key_pressed (da, da2) dw_ref ev = 
  let dw = !dw_ref in

  pr2 ("key pressed");

  (* this is in device coordinate, so no need to take into account the zoom *)
  let _delta_move = float dw.width /. 16. in
  let delta_move_user = 0.1 in (* TODO *)

  let delta_zoom = 1.3 in

  let b = 
    (match GdkEvent.Key.keyval ev with
    | k when k = K._Left ->
        dw.xtrans <- dw.xtrans +. delta_move_user;
        (* todo opti: *)
        paint dw;
        true
    | k when k = K._Right ->
        dw.xtrans <- dw.xtrans -. delta_move_user;
        (* todo opti: *)
        paint dw;
        true

    | k when k = K._Up ->
        dw.ytrans <- dw.ytrans +. delta_move_user;
        (* todo opti: *)
        paint dw;
        true
    | k when k = K._Down ->
        dw.ytrans <- dw.ytrans -. delta_move_user;
        (* todo opti: *)
        paint dw;
        true

    | k when k = K._plus ->
        dw.zoom <- dw.zoom /. delta_zoom;
        (* can't optimize here, have to paint *)
        paint dw;
        true
    | k when k = K._minus ->
        dw.zoom <- dw.zoom *. delta_zoom;
        (* can't optimize here, have to paint *)
        paint dw;
        true




    | k when k = K._z ->
        dw.in_zoom_incruste <- not (dw.in_zoom_incruste);
        true

    | k when k = K._b ->
        !Ctl._go_back dw_ref;
        true

    | k when k = K._e ->
        raise Todo

    | k when k = K._q ->
        GMain.quit () ; false

    | _ -> false
    )
  in
  if b then begin
    GtkBase.Widget.queue_draw da#as_widget;
    GtkBase.Widget.queue_draw da2#as_widget;
  end;
  b
*)
(*e: key_pressed *)

(*s: find_filepos_in_rectangle_at_user_point *)
(*e: find_filepos_in_rectangle_at_user_point *)
            

(*s: button_action *)
let button_action da dw_ref ev =
  let dw = !dw_ref in

  let x, y = GdkEvent.Button.x ev, GdkEvent.Button.y ev in
  let pt = { Cairo. x = x; y = y } in
  let user = with_map dw (fun cr -> Cairo.device_to_user cr pt) in
  let r_opt = M.find_rectangle_at_user_point dw user in

  match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->
      let button = GdkEvent.Button.button ev in
      let state = GdkEvent.Button.state ev in
      pr2 (spf "button %d pressed" button);
      (match button with
      | 1 -> 
        (* DISABLED FOR NOW
         * dw.drag_pt <- { 
         * Cairo.x = GdkEvent.Button.x ev; 
         * Cairo.y = GdkEvent.Button.y ev; 
         * };
         * dw.in_dragging <- true;
         *)
        r_opt +> Common.do_option (fun (r, _, _r_englobing) ->
          let file = r.T.tr_label in
          pr2 (spf "clicking on %s" file);
        );
        true
      | 2 ->
        r_opt +> Common.do_option (fun (r, _, _r_englobing) ->
          let file = r.T.tr_label in
          pr2 (spf "opening %s" file);
          let line =
            match M.find_line_in_rectangle_at_user_point dw user r with
            | None -> Line 0
            | Some l -> l
          in
          Editor_connection.open_file_in_current_editor ~file ~line;
        );
        true

      | 3 ->

        r_opt +> Common.do_option (fun (r, _, _r_englobing) ->
          let file = r.T.tr_label in

          if not (Gdk.Convert.test_modifier `SHIFT state)
          then !Ctl._go_dirs_or_file dw_ref [file]
          else begin

          (* similar to View_overlays.motion.refresher *)
          let entity_opt =
            M.find_line_in_rectangle_at_user_point dw user r >>= (fun line ->
              M.find_def_entity_at_line_opt line r dw)
          in
          
          let uses, users = 
            match entity_opt with
            | None -> M.deps_readable_files_of_file file dw
            | Some n -> M.deps_readable_files_of_node n dw
          in

          let model = Async.async_get dw.dw_model in
          let paths_of_readables xs = 
            xs 
            +> List.sort Pervasives.compare
            +> Common2.uniq
            (* todo: tfidf to filter files like common2.ml *)
            +> Common.exclude (fun readable -> 
                readable =~ "commons/common2.ml"
                (*readable =~ "external/.*"  *)
            )
            +> List.map (fun s -> Filename.concat model.root s)
            (* less: print a warning when does not exist? *)
            +> List.filter Sys.file_exists
          in
          let readable = Common.readable ~root:model.root file in
          let entries = [
            `I ("go to file", (fun () -> 
              !Ctl._go_dirs_or_file dw_ref (paths_of_readables [readable]);));
            `I ("deps inout", (fun () -> 
              !Ctl._go_dirs_or_file dw_ref (paths_of_readables 
                                              (uses ++ users ++ [readable]))));
            `I ("deps in (users)", (fun () -> 
              !Ctl._go_dirs_or_file dw_ref (paths_of_readables 
                                              (users ++ [readable]))));
            `I ("deps out (uses)", (fun () -> 
              !Ctl._go_dirs_or_file dw_ref (paths_of_readables 
                                            (uses ++ [readable]))));
          ] in
          let entries = 
            entries ++
            (match entity_opt, model.g with
            | None, _ -> []
            | Some e, Some g -> 
                [`I ("info entity", (fun () ->
                  let users = Graph_code.pred e (Graph_code.Use) g in
                  let str =
                    users +> List.map Graph_code.string_of_node 
                    +> Common.join "\n"
                  in
                  Gui.dialog_text ~text:str ~title:"Info entity";
                ))]
            | _ -> raise Impossible
            )
          in
          GToolbox.popup_menu ~entries ~button:3 
            ~time:(GtkMain.Main.get_current_event_time());
          end
        );
        true
      | _ -> false
      )
  | `BUTTON_RELEASE ->

      let button = GdkEvent.Button.button ev in
      pr2 (spf "button %d released" button);

      (match button with
      | 1 ->
          GtkBase.Widget.queue_draw da#as_widget;
          true
      | _ -> false
      )

  | `TWO_BUTTON_PRESS ->
      pr2 ("double click");

      r_opt +> Common.do_option (fun (_r, _, r_englobing) ->
        let path = r_englobing.T.tr_label in
        !Ctl._go_dirs_or_file dw_ref [path];
      );

      true
  | _ -> false
(*e: button_action *)


(*e: view_mainmap.ml *)
