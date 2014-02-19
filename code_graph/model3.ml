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

module CairoH = Cairo_helpers3
module DM = Dependencies_matrix_code
module DMBuild = Dependencies_matrix_build

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The code model *)
(*****************************************************************************)

type model = {
  (* unused for now *)
  root: Common.dirname;

  (* less: this graph may be really huge and may stress the GC
   * during interactions, so for now just use gopti ? I tried
   * and didn't see much difference :(
   * Moreover we now tend to modify gopti and so having a g
   * out of sync with the gopti is bad.
   *)
  g_deprecated: Graph_code.graph; 
  (* we dynamically modify the optimized graph to add some intermediate
   * 'a/b/...' directories
   *)
  mutable gopti: Graph_code_opti.graph;
  constraints: Dependencies_matrix_code.partition_constraints;
}

(*****************************************************************************)
(* The drawing model *)
(*****************************************************************************)

(* All the 'float's below are to be intepreted as user coordinates except when
 * explicitely mentioned. All the 'int's are usually device coordinates.
 *)
type world = {
  model: model;

  mutable path: Dependencies_matrix_code.config_path;
  (* cache of Dependencies_matrix_code.build (config_of_path path) g *)
  mutable m: Dependencies_matrix_code.dm;
  
  (* set each time in View_matrix.draw_matrix.
   * opti: use a quad tree?
   *)
  mutable interactive_regions: (region * Figures.rectangle) list;

  mutable base:    [ `Any ] Cairo.surface;
  mutable overlay: [ `Any ] Cairo.surface;

  (* viewport, device coordinates *)
  mutable width:  int;
  mutable height: int;
}
  and region =
    | Cell of int * int (* i, j *)
    | Row of int (* i *)
    | Column of int (* j *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let new_surface ~alpha ~width ~height =
  let drawable = GDraw.pixmap ~width:1 ~height:1 () in
  drawable#set_foreground `WHITE;
  drawable#rectangle ~x:0 ~y:0 ~width:1 ~height:1 ~filled:true ();

  let cr = Cairo_lablgtk.create drawable#pixmap in
  let surface = Cairo.get_target cr in
  Cairo.surface_create_similar surface
    (if alpha 
    then Cairo.CONTENT_COLOR_ALPHA
    else Cairo.CONTENT_COLOR
    ) width height

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* width/height are a first guess. The first configure ev will force a resize
 * coupling: with View_matrix.recompute_matrix
 *)
let init_world ?(width = 600) ?(height = 600) path model =
  let config, gopti = DMBuild.config_of_path path model.gopti in
  model.gopti <- gopti;
  let m, gopti = 
    Common.profile_code "Model.building matrix" (fun () -> 
      DMBuild.build config (Some model.constraints) model.gopti
    )
  in
  model.gopti <- gopti;
  {
    model; 
    path;
    interactive_regions = [];
    m;
    width; height;
    base    = new_surface ~alpha:false ~width ~height;
    overlay = new_surface ~alpha:false ~width ~height;
  }

(*****************************************************************************)
(* Coordinate system *)
(*****************************************************************************)

(* On my 30' monitor, when I run codegraph and expand it to take the
 * whole screen, then the Grab utility tells me that the drawing area
 * is 2560 x 1490 (on my laptop it's 1220 x 660).
 * So if we want a uniform coordinate system that is
 * still aware of the proportion (like I did in Treemap.xy_ratio),
 * then 1.71 x 1 is a good choice.
 *)
let xy_ratio = 1.71

(* 0..1.71 x 0..1 internal normalized coords -> width x height external pixels*)
let scale_coordinate_system cr w =
  Cairo.scale cr
    (float_of_int w.width / xy_ratio)
    (float_of_int w.height);
  ()

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* todo: can put some of it as mutable? as we expand things we may want
 * to reserve more space to certain things?
 *)
type layout = {
(* this assumes the xy_ratio set above *)
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
  let x_end_matrix_right = xy_ratio in
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

let find_region_at_user_point2 w ~x ~y =
  let regions = w.interactive_regions in
  let pt = { Figures. x = x; y = y } in
  regions +> Common.find_some_opt (fun (kind, rect) ->
      if Figures.point_is_in_rectangle pt rect
      then Some kind
      else None
  )

let find_region_at_user_point w ~x ~y =
  Common.profile_code "model.find_region" (fun () ->
    find_region_at_user_point2 w ~x ~y)
