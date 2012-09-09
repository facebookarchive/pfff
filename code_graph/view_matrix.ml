(* Yoann Padioleau
 * 
 * Copyright (C) 2012 Facebook
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
open Common.ArithFloatInfix

open Figures
module CairoH = Cairo_helpers3

open Model3
module M = Model3
module Ctl = Controller3

module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

 
(*****************************************************************************)
(* Drawing helpers *)
(*****************************************************************************)

let draw_cells cr w ~interactive_regions =
  let l = M.layout_of_w w in

  for i = 0 to l.nb_elts -.. 1 do
    for j = 0 to l.nb_elts -.. 1 do
      (* the matrix is accessed as matrix.(row).(col), but here y corresponds
       * to the row, and x to the column, hence the association of j to x
       * and i to y.
       *)
      let x = (float_of_int j) * l.width_cell + l.x_start_matrix_left in
      let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in

      let rect = { 
        p = { x = x; y = y; };
        q = { x = x + l.width_cell; y = y + l.height_cell };
      } in
      Common.push2 (Cell (i, j), rect) interactive_regions;
      
      (* less: could also display intra dependencies *)
      if i = j then
        CairoH.fill_rectangle_xywh ~cr ~x ~y ~w:l.width_cell ~h:l.height_cell
          ~color:"wheat" ()
      else begin
        CairoH.draw_rectangle ~cr ~line_width:0.0005 ~color:"wheat" rect;
        let n = w.m.DM.matrix.(i).(j) in
        if n > 0 then begin
          let txt = string_of_int n in
          let font_size = 
            match n with
            | _ when n <= 10 -> 
                l.width_cell / 2.
            | _ ->
                l.width_cell / (float_of_int (String.length txt))
          in
          CairoH.set_font_size cr font_size;
          (* todo: optimize? *)
          let extent = CairoH.text_extents cr txt in
          let tw = extent.Cairo.text_width in
          let th = extent.Cairo.text_height in
          
          let x = x + (l.width_cell / 2.) - (tw / 2.0) in
          let y = y + (l.height_cell / 2.) + (th / 2.0) in
          Cairo.move_to cr x y;
          CairoH.show_text cr txt;
        end;
      end
    done
  done;
  ()

let draw_left_rows cr w ~interactive_regions =
  let l = M.layout_of_w w in
  let font_size = 
    min (l.height_cell / 1.5) (l.x_start_matrix_left / 10. )
  in
  CairoH.set_font_size cr font_size;
  (* peh because it exercises the spectrum of high letters *)
  let extent = CairoH.text_extents cr "peh" in
  let _base_tw = extent.Cairo.text_width / 3. in
  let th = extent.Cairo.text_height in

  for i = 0 to l.nb_elts -.. 1 do
    let x = 0. in
    let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
    let rect = { 
      p = { x = x; y = y; };
      q = { x = l.x_start_matrix_left; y = y + l.height_cell };
    } in
    Common.push2 (Row i, rect) interactive_regions;
    CairoH.draw_rectangle ~cr ~line_width:0.0005 ~color:"wheat" rect;
    (* align on the left *)
    Cairo.move_to cr (x + 0.02) (y + (l.height_cell /2.) + (th / 2.0));
    let node = Hashtbl.find w.m.DM.i_to_name i in
    let (txt, _kind) = node in
    CairoH.show_text cr txt;
  done;
  ()

let draw_up_columns cr w ~interactive_regions =
  let l = M.layout_of_w w in

  (* peh because it exercises the spectrum of high letters *)
  let extent = CairoH.text_extents cr "peh" in
  let _base_tw = extent.Cairo.text_width / 3. in
  let th = extent.Cairo.text_height in

  (* not -.. 1, cos we draw lines here, not rectangles *)
  for j = 0 to l.nb_elts do
    let x = (float_of_int j) * l.width_cell + l.x_start_matrix_left in
    let y = l.y_start_matrix_up in
    let rect = {
      (* fake rectangle *)
      p = { x = x; y = 0. };
      q = { x = x + l.width_cell; y = l.y_start_matrix_up };
    } in
    Common.push2 (Column j, rect) interactive_regions;

    CairoH.set_source_color ~cr ~color:"wheat" ();
    Cairo.move_to cr x y;
    (* because of the xy_ratio, this actually does not do a 45 deg line.
     * old: Cairo.line_to cr (x + (y_start_matrix_up / atan (pi / 4.)))  0.; 
     *)
    Cairo.line_to cr (x + (l.y_start_matrix_up / atan (pi / 2.8)))  0.; 
    Cairo.stroke cr;

    if j < l.nb_elts then begin
      let node = Hashtbl.find w.m.DM.i_to_name j in
      let (txt, _kind) = node in

      
      Cairo.move_to cr (x + (l.width_cell / 2.0) + (th / 2.0)) (y - 0.001);
      let angle = -. (pi / 4.) in
      Cairo.rotate cr ~angle:angle;
      CairoH.show_text cr txt;
      Cairo.rotate cr ~angle:(-. angle);
    end;
  done;
  ()


(*****************************************************************************)
(* Drawing entry point *)
(*****************************************************************************)

(* assumes cr is setup with uniform coordinate system *)
let draw_matrix cr w =
  (* clear the screen *)
  CairoH.fill_rectangle_xywh ~cr ~x:0.0 ~y:0.0 ~w:xy_ratio ~h:1.0 
    ~color:"DarkSlateGray" ();

  let l = M.layout_of_w w in

  (* draw matrix enclosing rectangle *)
  CairoH.draw_rectangle ~cr ~line_width:0.001 ~color:"wheat"
    { p = { x = l.x_start_matrix_left; y = l.y_start_matrix_up };
      q = { x = l.x_end_matrix_right; y = 1.0 };
    };
  Cairo.select_font_face cr "serif"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;

  let interactive_regions = ref [] in


  draw_cells      cr w ~interactive_regions;
  draw_left_rows  cr w ~interactive_regions;
  draw_up_columns cr w ~interactive_regions;

  w.interactive_regions <- !interactive_regions;
  
  ()

(*****************************************************************************)
(* Painting entry point *)
(*****************************************************************************)

(* 'Paint' creates the cairo context and adjust the scaling if needed
 * and then call 'draw' functions.
 *)
let paint w =
  let cr = Cairo.create w.base in
  scale_coordinate_system cr w;
  draw_matrix cr w;
  !Ctl._refresh_drawing_area ();
  ()

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

let recompute_matrix w =
  let m = 
    Common.profile_code2 "Model.building matrix" (fun () -> 
      Dependencies_matrix_code.build w.config w.model.g 
    )
  in
  w.m <- m;
  paint w;
  ()

let button_action da w ev =
  let (x, y) = GdkEvent.Button.x ev, GdkEvent.Button.y ev in
  let pt = { Cairo. x = x; y = y } in
  pr2 (spf "button action device coord: %f, %f" x y);

  let cr = Cairo.create w.overlay in
  M.scale_coordinate_system cr w;

  let pt2 = Cairo.device_to_user cr pt in
  let (x, y) = (pt2.Cairo.x, pt2.Cairo.y) in
  pr2 (spf "button action user coord: %f, %f" x y);

  (match M.find_region_at_user_point w ~x ~y with
  | None -> false
  | Some x ->
      (match x with
      | Row i -> 
            (match GdkEvent.get_type ev, GdkEvent.Button.button ev with
            | `TWO_BUTTON_PRESS, 1 ->
                pr2 (spf "double clicking on row i");
                let node = Hashtbl.find w.m.DM.i_to_name i in
                w.config <- DM.expand_node node w.config w.model.g;
                recompute_matrix w;
                true
            | `BUTTON_PRESS, 3 ->
                pr2 (spf "right clicking on row i");
                let node = Hashtbl.find w.m.DM.i_to_name i in
                w.config <- 
                  DM.focus_on_node node DM.DepsOut w.config w.m;
                recompute_matrix w;
                true

            | `BUTTON_RELEASE, _ |  _ ->
                false
            )
      | Cell (i, j) -> 
            (match GdkEvent.get_type ev, GdkEvent.Button.button ev with
            | `BUTTON_PRESS, 1 ->
                pr2 (spf "clicking on cell (%d, %d)" i j);
                let deps = 
                  DM.explain_cell_list_use_edges (i, j) w.m w.model.g in
                let str = 
                  deps +> List.map (fun (n1, n2) ->
                    spf "%s --> %s"
                      (Graph_code.string_of_node n1)  
                      (Graph_code.string_of_node n2)
                  )
                  +> Common.join "\n"
                in
                Gui.dialog_text ~text:str ~title:"Cell explaination";
                true

            | `BUTTON_PRESS, 3 ->
                pr2 (spf "right clicking on cell (%d, %d)" i j);
                if i = j
                then begin
                  let node = Hashtbl.find w.m.DM.i_to_name j in
                  w.config <- 
                    DM.focus_on_node node DM.DepsInOut w.config w.m;
                  recompute_matrix w;
                  true
                end else
                  false

            | `BUTTON_RELEASE, _ | `TWO_BUTTON_PRESS, _ | _ ->
                false
            )

      | Column j ->
            (match GdkEvent.get_type ev, GdkEvent.Button.button ev with
            | `BUTTON_PRESS, 3 ->
                pr2 (spf "right clicking on column j");
                let node = Hashtbl.find w.m.DM.i_to_name j in
                w.config <- 
                  DM.focus_on_node node DM.DepsIn w.config w.m;
                recompute_matrix w;
                true

            | _ -> false
            )
      )
  )

