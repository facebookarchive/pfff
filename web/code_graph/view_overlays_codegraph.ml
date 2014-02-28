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
open Common2.ArithFloatInfix
open Common_client

open Figures
open Model_codegraph
module M = Model_codegraph

module G = Graph_code
module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
let color_using = "green"
let color_used_by = "chocolate"

(*****************************************************************************)
(* The overlays *)
(*****************************************************************************)

(* inspired by the DSM in jetbrains IDEA *)
let draw_row_column ~(ctx: Canvas_helpers.context) ~color w i =
  let l = M.layout_of_w w in

  (* draw row *)
  let x = 0. in
  let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
  let rect = { 
    p = { x = x; y = y; };
    q = { x = l.x_end_matrix_right; y = y + l.height_cell };
  } in
  ctx#fill_rectangle ~color ~alpha:0.3 rect;

  (* draw column *)
  let x = (float_of_int i) * l.width_cell + l.x_start_matrix_left in
  let y = l.y_start_matrix_up in

  let rect = { 
    p = { x = x; y = y; };
    q = { x = x + l.width_cell; y = 1.0 };
  } in
  ctx#fill_rectangle ~color ~alpha:0.3 rect;
  ()
 

(* inspired by the DSM in jetbrains IDEA *)
let draw_green_yellow_dependent_rows ~(ctx: Canvas_helpers.context) w i =
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
    ctx#fill_rectangle ~color:color_using ~alpha:0.3 rect;
  );

  !i_used_by +> List.iter (fun i ->
    let x = l.x_start_matrix_left - 0.05 in
    let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
    let rect = { 
      p = { x = x; y = y; };
      q = { x = l.x_start_matrix_left; y = y + l.height_cell };
    } in
    ctx#fill_rectangle ~color:color_used_by ~alpha:0.3 rect;
  );
  ()


(*****************************************************************************)
(* Assembling overlays *)
(*****************************************************************************)

(* was called motion_notify_refresher in gtk version *)
let mousemove
 (ctx: Canvas_helpers.context) (w: Model_codegraph.world_client) 
 elt ev =
  let device_x, device_y = Common_client.get_position elt ev in
  pr2 (spf "mousemove device coord: %d x %d" device_x device_y);

  (* clear overlay *)
  ctx#canvas_ctx##clearRect (0., 0., M.xy_ratio, 1.0);

  let (x, y) = ctx#device_to_user ~x:device_x ~y:device_y in
  pr2 (spf "motion user coord: %f, %f" x y);

  (* less: update status bar? *)
  (match M.find_region_at_user_point w ~x ~y with
  | None -> ()
  | Some x ->
      (match x with
      | Row i -> 
          draw_green_yellow_dependent_rows ~ctx w i;
          draw_row_column ~color:color_used_by ~ctx w i;
(*
          let txt = spf "Row: %s" 
            (G.string_of_node (w.m.DM.i_to_name.(i))) in
          !Ctl._statusbar_addtext txt;
*)
      | Cell (i, j) ->
          draw_green_yellow_dependent_rows ~ctx w i;
          draw_row_column ~color:color_used_by ~ctx w i;
          draw_row_column ~color:color_using ~ctx w j;
(*
          let txt = spf "Cell: %s x %s" 
            (G.string_of_node (w.m.DM.i_to_name.(i)))
            (G.string_of_node (w.m.DM.i_to_name.(j)))
          in
          !Ctl._statusbar_addtext txt;
*)
      | Column j ->
          draw_green_yellow_dependent_rows ~ctx w j;
          draw_row_column ~color:color_using ~ctx w j;
(*
          let txt = spf "Col: %s" 
            (G.string_of_node (w.m.DM.i_to_name.(j))) in
          !Ctl._statusbar_addtext txt;
*)

      )
  );
  ()


(*
let motion_notify da w ev =
  !Ctl.current_motion_refresher +> Common.do_option (fun x ->
    GMain.Idle.remove x;
  );
  Ctl.current_motion_refresher := 
    Some (Gui.gmain_idle_add ~prio:100 (motion_notify_refresher da w ev));
  true

*)
