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

module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize with pfff/code_graph/model3.ml *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type world_client = {
  project: string;
  (* todo: Dependencies_matrix_code.config_path at some point *)
  path: string list; 
  size: string;

  m: Dependencies_matrix_code.dm;

  (* this is too big, keep on server, and it looks like it's not marshalled
   * correctly as some Hashtbl.find failed on the passed data in the client
   * whereas it works perfectly fine on the same data on the server
   *)
  (* gopti: Graph_code_opti.graph; *)

  (* viewport, device coordinates *)
  width:  int;
  height: int;

  mutable interactive_regions: (region * Figures.rectangle) list;
}

and region =
    | Cell of int * int (* i, j *)
    | Row of int (* i *)
    | Column of int (* j *)

(*****************************************************************************)
(* Links *)
(*****************************************************************************)

(*****************************************************************************)
(* Coordinate system *)
(*****************************************************************************)

let xy_ratio = 1.71

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* less: can put some of it as mutable? as we expand things we may want
 * to reserve more space to certain things?
 *)
type layout = {
(* this assumes a xy_ratio of 1.71 *)
  x_start_matrix_left: float;
  x_end_matrix_right: float;
  y_start_matrix_up: float;
  y_end_matrix_down: float;

  width_vertical_label: float;

  nb_elts: int;
  width_cell: float;
  height_cell: float;
}

let layout_of_w w = 
  let x_start_matrix_left = 0.3 in
  let x_end_matrix_right = 1.70 in
  (* this will be with 45 degrees so it can be less than x_start_matrix_left *)
  let y_start_matrix_up = 0.1 in
  let y_end_matrix_down = 1.0 in

  let nb_elts = Array.length w.m.DM.matrix in
  let width_cell = 
    (x_end_matrix_right - x_start_matrix_left) / (float_of_int nb_elts) in
  let height_cell = 
    (1.0 - y_start_matrix_up) / (float_of_int nb_elts) in
  {
    x_start_matrix_left;
    x_end_matrix_right;
    y_start_matrix_up;
    y_end_matrix_down;

    width_vertical_label = 0.025;

    nb_elts;
    width_cell;
    height_cell;
  }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let find_region_at_user_point w ~x ~y =
  let regions = w.interactive_regions in
  let pt = { Figures. x = x; y = y } in
  try 
    let (kind, rect) = regions +> List.find (fun (kind, rect) ->
      Figures.point_is_in_rectangle pt rect
    )
    in
    Some kind
  with Not_found -> None
