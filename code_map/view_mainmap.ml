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

open Model2
module CairoH = Cairo_helpers
module F = Figures
module T = Treemap
module Flag = Flag_visual
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
  let cr = Cairo.create dw.base in
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
let paint_content_maybe_rect ~user_rect dw model rect =
 with_map dw (fun cr ->
   let context = M.context_of_drawing dw model in
   let clipping = user_rect in
   let microlevel_opt = 
     Draw_microlevel.draw_treemap_rectangle_content_maybe
       cr clipping context rect in
   microlevel_opt +> Common.do_option (fun microlevel ->
     Hashtbl.replace dw.microlevel rect microlevel
   );
  (* have to redraw the label *)
   Draw_labels.draw_treemap_rectangle_label_maybe 
     ~cr ~zoom:1.0 ~color:None rect;
 )

(* todo: deadlock:  M.locked (fun () ->  ) dw.M.model.M.m *)
let lazy_paint user_rect dw model () =
  pr2 "Lazy Paint";
  let start = Unix.gettimeofday () in
  while Unix.gettimeofday () - start < 0.3 do
    match !Ctl.current_rects_to_draw with
    | [] -> ()
    | x::xs ->
        Ctl.current_rects_to_draw := xs;
        pr2 (spf "Drawing: %s" (x.T.tr_label));
        paint_content_maybe_rect ~user_rect dw model x;
  done;
  !Ctl._refresh_da ();
  if !Ctl.current_rects_to_draw = []
  then begin
    !Ctl.hook_finish_paint ();
    !Ctl._refresh_da ();
    false
  end
  (* call me again *)
  else true

let paint2 dw model = 
  pr2 (spf "paint");
  
  !Ctl.paint_content_maybe_refresher +> Common.do_option GMain.Idle.remove;
  Ctl.current_rects_to_draw := [];

  let user_rect = device_to_user_area dw in
  pr2 (F.s_of_rectangle user_rect);
  let nb_rects = dw.nb_rects in
  let rects = dw.treemap in

  with_map dw (fun cr ->

    (if not (Layer_code.has_active_layers dw.layers)
     (* phase 1, draw the rectangles *)
     then rects +> List.iter (Draw_macrolevel.draw_treemap_rectangle ~cr)
     else rects +> List.iter (Draw_macrolevel.draw_trect_using_layers 
                                ~cr dw.layers)
    );

    (* phase 2, draw the labels, if have enough space *)
    rects +> List.iter (Draw_labels.draw_treemap_rectangle_label_maybe 
                          ~cr ~zoom:1.0  ~color:None);
  );

  (* phase 3, draw the content, if have enough space *)
  if nb_rects < !Flag.threshold_nb_rects_draw_content
  (* draw_content_maybe calls nblines which is quite expensive so
   * want to limit it *)
  then begin
    Ctl.current_rects_to_draw := rects;
    Ctl.paint_content_maybe_refresher := 
        Some (Gui.gmain_idle_add ~prio:3000 (fun () ->
          let res = lazy_paint user_rect dw model () in
          Ctl.paint_content_maybe_refresher := None;
          res
        ));
  end

let paint a b = 
  Common.profile_code "View.paint" (fun () -> paint2 a b)
(*e: paint *)

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

(*s: key_pressed *)
(*e: key_pressed *)
(*s: find_filepos_in_rectangle_at_user_point *)
(*e: find_filepos_in_rectangle_at_user_point *)
   
(*s: button_action *)
let button_action w ev =
  let dw = w.dw in

  let x, y = GdkEvent.Button.x ev, GdkEvent.Button.y ev in
  let pt = { Cairo. x = x; y = y } in
  let user = with_map dw (fun cr -> Cairo.device_to_user cr pt) in
  let r_opt = M.find_rectangle_at_user_point user dw in

  match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->
      let button = GdkEvent.Button.button ev in
      let state = GdkEvent.Button.state ev in
      pr2 (spf "button %d pressed" button);
      (match button with
      | 1 -> 
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
            M.find_line_in_rectangle_at_user_point user r dw ||| (Line 0)
          in
          Editor_connection.open_file_in_current_editor ~file ~line;
        );
        true

      | 3 ->
        r_opt +> Common.do_option (fun (tr, _, _r_englobing) ->
          (* actually file_or_dir *)
          let file = tr.T.tr_label in

          if not (Gdk.Convert.test_modifier `SHIFT state)
          then !Ctl._go_dirs_or_file w [file]
          else begin

          let model = Async.async_get w.model in

          (* similar to View_overlays.motion.refresher *)
          let line_opt = 
            M.find_line_in_rectangle_at_user_point user tr dw in
          let glyph_opt =
            M.find_glyph_in_rectangle_at_user_point user tr dw in
          let entity_def_opt = 
            line_opt >>= (fun line ->
              M.find_def_entity_at_line_opt line tr dw model) in
          let entity_use_opt =
            line_opt >>= (fun line -> 
            glyph_opt >>= (fun glyph ->
              M.find_use_entity_at_line_and_glyph_opt line glyph tr dw model))
          in
          let entity_opt = 
            match entity_use_opt, entity_def_opt with
            (* priority to use *)
            | Some e, Some _ -> Some e
            | Some e, _ | _, Some e -> Some e
            | _ -> None
          in
          let n = entity_opt ||| M.node_of_rect tr model in
        
          let uses, users = M.deps_readable_files_of_node n model in

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
          let readable =
            match entity_opt with
            | None -> readable
            | Some n -> 
                let g = Common2.some model.g in
                try Graph_code.file_of_node n g
                with Not_found -> readable
          in
          let entries = [
            `I ("go to file", (fun () -> 
              !Ctl._go_dirs_or_file w (paths_of_readables [readable]);));
            `I ("deps inout", (fun () -> 
              w.current_node_selected <- entity_opt;
              !Ctl._go_dirs_or_file w (paths_of_readables 
                                         (uses @ users @ [readable]))));
            `I ("deps in (users)", (fun () -> 
              w.current_node_selected <- entity_opt;
              !Ctl._go_dirs_or_file w (paths_of_readables (users@[readable]))));
            `I ("deps out (uses)", (fun () -> 
              w.current_node_selected <- entity_opt;
              !Ctl._go_dirs_or_file w (paths_of_readables (uses@[readable]))));
            `I ("info file", (fun () -> 
              let microlevel = Hashtbl.find dw.microlevel tr in
              let defs = microlevel.defs in
              let buf = Buffer.create 100 in
              let prx s = Buffer.add_string buf (s ^ "\n") in
              prx "short defs";
              defs +> List.iter (fun (_line, short_node) ->
                  prx (" " ^ Graph_code.string_of_node short_node)
              );
              Gui.dialog_text ~text:(Buffer.contents buf) ~title:"Info file";
            ));
          ] in
          let entries = 
            entries @
            (match entity_opt with
            | None -> []
            | Some n ->
                let g = Common2.some model.g in
                [`I ("info entity", (fun () ->
                  let users = Graph_code.pred n (Graph_code.Use) g in
                  let str =
                    ([Graph_code.string_of_node n] @
                    (users +> List.map Graph_code.string_of_node )
                    ) +> Common.join "\n"
                  in
                  Gui.dialog_text ~text:str ~title:"Info entity";
                ));
                 `I ("goto def", (fun () ->
                   w.current_node_selected <- entity_opt;
                   let dest = Graph_code.file_of_node n g in
                   !Ctl._go_dirs_or_file w (paths_of_readables [dest])
                 ));                                              
                ]
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
      | 1 -> true
      | _ -> false
      )

  | `TWO_BUTTON_PRESS ->
      pr2 ("double click");
      r_opt +> Common.do_option (fun (_r, _, r_englobing) ->
        let path = r_englobing.T.tr_label in
        !Ctl._go_dirs_or_file w [path];
      );
      true
  | _ -> false
(*e: button_action *)

(*e: view_mainmap.ml *)
