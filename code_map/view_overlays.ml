(*s: view_overlays.ml *)
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
module F = Figures
module T = Treemap
module CairoH = Cairo_helpers
module M = Model2
module Controller = Controller2
module Style = Style2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module mainly modifies the dw.overlay cairo surface. It also
 * triggers the refresh_da which triggers itself the expose event
 * which triggers the View2.assemble_layers composition of dw.pm with
 * dw.overlay.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let readable_txt_for_label txt current_root =
  let readable_txt = 
    if current_root =$= txt (* when we are fully zoomed on one file *)
    then "root"
    else Common.readable ~root:current_root txt 
  in
  if String.length readable_txt > 25
  then 
    let dirs = Filename.dirname readable_txt +> Common.split "/" in
    let file = Filename.basename readable_txt in
    spf "%s/.../%s" (List.hd dirs) file
  else readable_txt

let with_overlay dw f =
  let cr_overlay = Cairo.create dw.overlay in
  View_mainmap.zoom_pan_scale_map cr_overlay dw;
  f cr_overlay

(*****************************************************************************)
(* The overlays *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* The current filename *)
(* ---------------------------------------------------------------------- *)
(*s: draw_label_overlay *)
(* assumes cr_overlay has not been zoom_pan_scale *)
let draw_label_overlay ~cr_overlay ~x ~y txt =

  Cairo.select_font_face cr_overlay "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size cr_overlay Style.font_size_filename_cursor;
      
  let extent = CairoH.text_extents cr_overlay txt in
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
  CairoH.show_text cr_overlay txt;
  ()
(*e: draw_label_overlay *)

(* ---------------------------------------------------------------------- *)
(* The current rectangles *)
(* ---------------------------------------------------------------------- *)

(*s: draw_rectangle_overlay *)
let draw_englobing_rectangles_overlay ~dw (r, middle, r_englobing) =
 with_overlay dw (fun cr_overlay ->
  CairoH.draw_rectangle_figure 
    ~cr:cr_overlay ~color:"white" r.T.tr_rect;
  CairoH.draw_rectangle_figure
    ~cr:cr_overlay ~color:"blue" r_englobing.T.tr_rect;

  Draw_labels.draw_treemap_rectangle_label_maybe 
    ~cr:cr_overlay ~color:(Some "red") ~zoom:1.0 r_englobing;

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
      ~cr:cr_overlay ~color:(Some color) ~zoom:1.0 r;
  );
 )
(*e: draw_rectangle_overlay *)

(* ---------------------------------------------------------------------- *)
(* Uses and users macrolevel *)
(* ---------------------------------------------------------------------- *)
let draw_deps_files tr dw model =
 with_overlay dw (fun cr_overlay ->
   let uses_rect, users_rect = M.deps_rects_of_rect tr dw model in
   (* todo: glowing layer *)
   uses_rect +> List.iter (fun r ->
     CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"green" r.T.tr_rect;
   );
   users_rect +> List.iter (fun r ->
     CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"red" r.T.tr_rect;
   )
 )

(* ---------------------------------------------------------------------- *)
(* Uses and users microlevel *)
(* ---------------------------------------------------------------------- *)
(* todo: better fisheye, with good background color *)
let draw_magnify_line_overlay_maybe ?honor_color dw line microlevel =
  with_overlay dw (fun cr_overlay ->
    let font_size = microlevel.layout.lfont_size in
    let font_size_real = CairoH.user_to_device_font_size cr_overlay font_size in

    (* todo: put in style *)
    if font_size_real < 5.
    then Draw_microlevel.draw_magnify_line 
          ?honor_color cr_overlay line microlevel
  )

let draw_deps_entities n dw model =
 with_overlay dw (fun cr_overlay ->

   line_and_microlevel_of_node_opt n dw model 
   +> Common.do_option (fun (_n2, line, microlevel) ->
     let rectangle = microlevel.line_to_rectangle line in
     CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"white" rectangle
   );

   let uses, users = M.deps_nodes_of_node_clipped n dw model in
   uses +> List.iter (fun (_n2, line, microlevel) ->
     let rectangle = microlevel.line_to_rectangle line in
     CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"green" rectangle;
   );
   users +> List.iter (fun (_n2, line, microlevel) ->
     let rectangle = microlevel.line_to_rectangle line in
     CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"red" rectangle;
     
     let lines_used = M.lines_where_used_node n line microlevel in
     lines_used +> List.iter (fun line ->
       let rectangle = microlevel.line_to_rectangle line in
       CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"purple" rectangle;

       draw_magnify_line_overlay_maybe ~honor_color:false dw line microlevel;
     );
   );
 )

(* ---------------------------------------------------------------------- *)
(* Tooltip/hovercard current entity *)
(* ---------------------------------------------------------------------- *)
(* assumes cr_overlay has not been zoom_pan_scale *)
let draw_tooltip ~cr_overlay ~x ~y n g =

  let pred = Graph_code.pred n Graph_code.Use g in
  let succ = Graph_code.succ n Graph_code.Use g in
  let files = 
    pred 
    +> Common.map_filter (fun n ->
        Common2.optionise (fun () -> (Graph_code.file_of_node n g)))
    +> Common.sort +> Common2.uniq
  in
  let str = spf "
 Entity: %s
 #Users: %d (%d different files)
 #Uses: %d
" (Graph_code.string_of_node n) 
    (List.length pred) (List.length files)
    (List.length succ)
  in
  let xs = Common2.lines str in

  (* copy paste of draw_label_overlay *)
  Cairo.select_font_face cr_overlay "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size cr_overlay Style.font_size_filename_cursor;

  let template = "peh" in
  let max_length = 
    xs +> List.map (String.length) +> Common2.maximum +> float_of_int in

  let extent = CairoH.text_extents cr_overlay template in
  let tw = extent.Cairo.text_width * ((max_length / 3.) +> ceil) in
  let th = extent.Cairo.text_height * 1.2 in

  let nblines = List.length xs +> float_of_int in
  let refx = x - tw / 2. in
  let refy = y - (th * nblines) in

  CairoH.fill_rectangle ~cr:cr_overlay 
    ~x:(refx + extent.Cairo.x_bearing) ~y:(refy + extent.Cairo.y_bearing)
    ~w:tw ~h:(th * nblines)
    ~color:"black"
    ~alpha:0.5
    ();

  Cairo.set_source_rgba cr_overlay 1. 1. 1.    1.0;
  xs +> Common.index_list_0 +> List.iter (fun (txt, line) ->
    let line = float_of_int line in
    Cairo.move_to cr_overlay refx (refy + line * th);
    CairoH.show_text cr_overlay txt;
  );
  ()

(* ---------------------------------------------------------------------- *)
(* The selected rectangles *)
(* ---------------------------------------------------------------------- *)

(*s: draw_searched_rectangles *)
let draw_searched_rectangles ~dw =
 with_overlay dw (fun cr_overlay ->
  dw.current_searched_rectangles +> List.iter (fun r ->
    CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"yellow" r.T.tr_rect
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
 )
(*e: draw_searched_rectangles *)

(*s: zoomed_surface_of_rectangle *)
(*e: zoomed_surface_of_rectangle *)

(*****************************************************************************)
(* Assembling overlays *)
(*****************************************************************************)

let paint_initial dw =
  let cr_overlay = Cairo.create dw.overlay in
  CairoH.clear cr_overlay
  (* can't do draw_deps_entities w.current_node here because
   * of lazy_paint(), the file content will not be ready yet
   *)

(* a bit ugly, but have to because of lazy_paint optimization *)
let hook_finish_paint w =
  (* pr2 "Hook_finish_paint"; *)
  let dw = w.dw in
  w.current_node +> Common.do_option (fun n -> 
    Async.async_get_opt w.model +> Common.do_option (fun model ->
      draw_deps_entities n dw model
    ));
  w.current_node_selected +> Common.do_option (fun n -> 
    Async.async_get_opt w.model +> Common.do_option (fun model ->
      draw_deps_entities n dw model
    ))

(*s: motion_refresher *)
let motion_refresher ev w =
  paint_initial w.dw;
  hook_finish_paint w;
  let dw = w.dw in
  let cr_overlay = Cairo.create dw.overlay in

  (* some similarity with View_mainmap.button_action handler *)
  let x, y = GdkEvent.Motion.x ev, GdkEvent.Motion.y ev in
  let pt = { Cairo. x = x; y = y } in
  let user = View_mainmap.with_map dw (fun cr -> Cairo.device_to_user cr pt) in
  let r_opt = M.find_rectangle_at_user_point user dw in

  r_opt +> Common.do_option (fun (tr, middle, r_englobing) ->
    (* coupling: similar code in right click handler in View_mainmap *)
    let line_opt = 
      M.find_line_in_rectangle_at_user_point user tr dw in
    let glyph_opt =
      M.find_glyph_in_rectangle_at_user_point user tr dw in

    let entity_def_opt = 
      Async.async_get_opt w.model >>= (fun model ->
      line_opt >>= (fun line ->
        M.find_def_entity_at_line_opt line tr dw model)) in
    let entity_use_opt =
      Async.async_get_opt w.model >>= (fun model ->
      line_opt >>= (fun line -> 
      glyph_opt >>= (fun glyph ->
        M.find_use_entity_at_line_and_glyph_opt line glyph tr dw model)))
    in
    let entity_opt = 
      match entity_use_opt, entity_def_opt with
      (* priority to use *)
      | Some e, Some _ -> Some e
      | Some e, _ | _, Some e -> Some e
      | _ -> None
    in

    let statusbar_txt = 
      tr.T.tr_label ^
      (match line_opt with None -> "" | Some (Line i) -> 
        spf ":%d" i) ^
      (match glyph_opt with None -> "" | Some glyph -> 
        spf "[%s]" glyph.str) ^
      (match entity_def_opt with None -> "" | Some n -> 
        spf "(%s)" (Graph_code.string_of_node n)) ^
      (match entity_use_opt with None -> "" | Some n -> 
        spf "{%s}" (Graph_code.string_of_node n))
    in
    !Controller._statusbar_addtext statusbar_txt;

    (match line_opt with
    | None ->
        let label_txt = readable_txt_for_label tr.T.tr_label dw.current_root in
        draw_label_overlay ~cr_overlay ~x ~y label_txt
    | Some line ->
        let microlevel = Hashtbl.find dw.microlevel tr in
        draw_magnify_line_overlay_maybe ~honor_color:true dw line microlevel
    );

    draw_englobing_rectangles_overlay ~dw (tr, middle, r_englobing);
    Async.async_get_opt w.model +> Common.do_option (fun model ->
      draw_deps_files tr dw model;
      entity_opt +> Common.do_option (fun _n -> w.current_node <- None);
      entity_def_opt+>Common.do_option (fun n -> draw_deps_entities n dw model);
      entity_use_opt+>Common.do_option (fun n -> draw_deps_entities n dw model);
    );
  
    if w.settings.draw_searched_rectangles;
    then draw_searched_rectangles ~dw;

    !Controller.current_tooltip_refresher
      |> Common.do_option GMain.Timeout.remove;
    Controller.current_tooltip_refresher := 
      Some (Gui.gmain_timeout_add ~ms:1000 ~callback:(fun _ ->
        Async.async_get_opt w.model +> Common.do_option (fun model ->
          match entity_opt, model.g with
          | Some node, Some g ->
            draw_tooltip ~cr_overlay ~x ~y node g;
            !Controller._refresh_da ();
          | _ -> ()
          ;
        );
        Controller.current_tooltip_refresher := None;
        (* do not run again *)
        false
      ));
    
    Controller.current_r := Some tr;
  );
  !Controller._refresh_da ();
  false


let motion_notify w ev =
(*  let x, y = GdkEvent.Motion.x ev, GdkEvent.Motion.y ev in *)
(*  pr2 (spf "motion: %f, %f" x y); *)

  (* The motion code now takes time, so it's better do run it when the user
   * has finished moving his mouse, hence the use of gmain_idle_add below.
   *)
  !Controller.current_motion_refresher +> Common.do_option GMain.Idle.remove;
  Controller.current_motion_refresher := 
    Some (Gui.gmain_idle_add ~prio:200 (fun () -> 
      let res = motion_refresher ev w in
      Controller.current_motion_refresher := None;
      res
    ));
  true
(*e: motion_refresher *)

(*s: idle *)
(*e: idle *)

(*e: view_overlays.ml *)
