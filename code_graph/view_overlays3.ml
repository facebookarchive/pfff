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
open Common2.ArithFloatInfix

open Figures
open Model3
module M = Model3
module Ctl = Controller3

module G = Graph_code
module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This module mainly modifies the w.overlay cairo surface. It also
 * triggers the refresh_da() which triggers itself the expose event
 * which triggers the View.assemble_layers composition of w.base with
 * w.overlay which refreshes the screen.
 *)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let color_using = "green"
let color_used_by = "chocolate"

(*****************************************************************************)
(* The overlays *)
(*****************************************************************************)

(* inspired by the DSM in jetbrains IDEA *)
let draw_row_column ~cr ~color w i =
  let l = M.layout_of_w w in

  (* draw row *)
  let x = 0. in
  let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
  let rect = { 
    p = { x = x; y = y; };
    q = { x = l.x_end_matrix_right; y = y + l.height_cell };
  } in
  CairoH.fill_rectangle ~cr ~color ~alpha:0.3 rect;

  (* draw column *)
  let x = (float_of_int i) * l.width_cell + l.x_start_matrix_left in
  let y = l.y_start_matrix_up in

  let rect = { 
    p = { x = x; y = y; };
    q = { x = x + l.width_cell; y = 1.0 };
  } in
  CairoH.fill_rectangle ~cr ~color ~alpha:0.3 rect;
  ()
 

(* inspired by the DSM in jetbrains IDEA *)
let draw_green_yellow_dependent_rows ~cr w i =
  let i_uses = ref [] in
  let i_used_by = ref [] in
  
  let l = M.layout_of_w w in

  for j = 0 to l.nb_elts -.. 1 do
    if w.m.DM.matrix.(i).(j) > 0 
    then Common.push j i_uses;
  done;

  for i2 = 0 to l.nb_elts -.. 1 do
    if w.m.DM.matrix.(i2).(i) > 0
    then Common.push i2 i_used_by;
  done;

  !i_uses +> List.iter (fun i ->
    let x = l.x_start_matrix_left - 0.05 in
    let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
    let rect = { 
      p = { x = x; y = y; };
      q = { x = l.x_start_matrix_left; y = y + l.height_cell };
    } in
    CairoH.fill_rectangle ~cr ~color:color_using ~alpha:0.3 rect;
  );

  !i_used_by +> List.iter (fun i ->
    let x = l.x_start_matrix_left - 0.05 in
    let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
    let rect = { 
      p = { x = x; y = y; };
      q = { x = l.x_start_matrix_left; y = y + l.height_cell };
    } in
    CairoH.fill_rectangle ~cr ~color:color_used_by ~alpha:0.3 rect;
  );
  ()


(*****************************************************************************)
(* Assembling overlays *)
(*****************************************************************************)

let motion_notify_refresher _da w ev () =

  let (x, y) = GdkEvent.Motion.x ev, GdkEvent.Motion.y ev in
  let pt = { Cairo. x = x; y = y } in
  pr2 (spf "motion device coord: %f, %f" x y);

  let cr = Cairo.create w.overlay in
  M.scale_coordinate_system cr w;

  (* clear overlay *)
  CairoH.clear cr;

  let pt2 = Cairo.device_to_user cr pt in
  let (x, y) = (pt2.Cairo.x, pt2.Cairo.y) in
  pr2 (spf "motion user coord: %f, %f" x y);

  (* less: update status bar? *)
  (match M.find_region_at_user_point w ~x ~y with
  | None -> ()
  | Some x ->
      (match x with
      | Row i -> 
          draw_green_yellow_dependent_rows ~cr w i;
          draw_row_column ~color:color_used_by ~cr w i;
          let txt = spf "Row: %s" 
            (G.string_of_node (w.m.DM.i_to_name.(i))) in
          !Ctl._statusbar_addtext txt;
      | Cell (i, j) ->
          draw_green_yellow_dependent_rows ~cr w i;
          draw_row_column ~color:color_used_by ~cr w i;
          draw_row_column ~color:color_using ~cr w j;
          let txt = spf "Cell: %s x %s" 
            (G.string_of_node (w.m.DM.i_to_name.(i)))
            (G.string_of_node (w.m.DM.i_to_name.(j)))
          in
          !Ctl._statusbar_addtext txt;
      | Column j ->
          draw_green_yellow_dependent_rows ~cr w j;
          draw_row_column ~color:color_using ~cr w j;
          let txt = spf "Col: %s" 
            (G.string_of_node (w.m.DM.i_to_name.(j))) in
          !Ctl._statusbar_addtext txt;

      )
  );
  !Ctl._refresh_drawing_area ();
  false

let motion_notify da w ev =
  !Ctl.current_motion_refresher +> Common.do_option GMain.Idle.remove;
  Ctl.current_motion_refresher := 
    Some (Gui.gmain_idle_add ~prio:100 (fun () -> 
      let res = motion_notify_refresher da w ev () in
      Ctl.current_motion_refresher := None;
      res
    ));
  true

