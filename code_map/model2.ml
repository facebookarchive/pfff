(*s: model2.ml *)
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

module CairoH = Cairo_helpers

module F = Figures
module T = Treemap

module Db = Database_code

module Flag = Flag_visual

(*****************************************************************************)
(* The code model *)
(*****************************************************************************)

(*s: type model *)
type model = {
  root: Common.dirname;

  db: Database_code.database option;
  (*s: model fields hook *)
    (* fast accessors *)
    hentities : (string, Database_code.entity) Hashtbl.t;
  (*x: model fields hook *)
    hfiles_entities : (Common.filename, Database_code.entity list) Hashtbl.t;
  (*x: model fields hook *)
    big_grep_idx: Big_grep.index;
  (*e: model fields hook *)

  (* for microlevel *)
  g: Graph_code.graph option;
  (* fast accessors, for macrolevel  *)
  huses_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  husers_of_file: (Common.filename, Common.filename list) Hashtbl.t;
 }
(*e: type model *)


(*****************************************************************************)
(* The drawing model *)
(*****************************************************************************)

(*s: type drawing *)
(* All the 'float' below are to be intepreted as user coordinates except when
 * explicitely mentioned. All the 'int' are usually device coordinates.
 *)
type drawing = {

  (* In user coordinates from 0 to T.xy_ratio and 1 for respectivey x and y.
   * Assumes the treemap contains absolute paths.
  *)
  treemap: Treemap.treemap_rendering;
  (* coupling: = List.length treemap *)
  nb_rects: int; 

  (* when we render content at the microlevel, we then need to know to which
   * line corresponds a position and vice versa.
   *)
  pos_and_line: (Treemap.treemap_rectangle, pos_and_line) Hashtbl.t;

  (* generated from dw.treemap, contains readable path relative to model.root *)
  readable_file_to_rect: 
    (Common.filename, Treemap.treemap_rectangle) Hashtbl.t;

  (* to compute zoomed treemap when double click *)
  treemap_func: Common.path list -> Treemap.treemap_rendering;

  (* This is to display readable paths. When fully zoomed it's a filename *)
  current_root: Common.path;

  (* computed lazily *)
  dw_model: model Async.t;

  mutable layers: Layer_code.layers_with_index;

  (*s: fields drawing query stuff *)
    (* queries *)
    mutable current_query: string;
    mutable current_searched_rectangles: Treemap.treemap_rectangle list;
    mutable current_entity: Database_code.entity option;
    mutable current_grep_query: (Common.filename, int) Hashtbl.t;
  (*e: fields drawing query stuff *)

  dw_settings: settings;

  (*s: fields drawing main view *)
    (* device coordinates *)
    mutable pm: GDraw.pixmap;
    mutable overlay: [ `Any ] Cairo.surface;

    (* todo: going from a point to the enclosing rectangle via pixel color
     *  trick. Kind of ugly.
     * mutable pm_color_trick: GDraw.pixmap;
     * mutable pm_color_trick_info: (string) array.
     * alternative: just find pixel by iterating over all the rectangles
     * and check if he's inside
     *)
  (*e: fields drawing main view *)

  (*s: fields drawing viewport *)
    (* viewport, device coordinates *)
    mutable width: int;
    mutable height: int;

    mutable zoom: float;

    (* in user coordinates *)
    mutable xtrans: float;
    mutable ytrans: float;

    mutable drag_pt: Cairo.point;
    mutable in_dragging: bool;

    mutable in_zoom_incruste: bool;
  (*e: fields drawing viewport *)

  (*s: fields drawing minimap *)
    (* minimap *)
    mutable pm_minimap: GDraw.pixmap;
    mutable width_minimap: int;
    mutable height_minimap: int;

    mutable drag_pt_minimap: Cairo.point;
  (*e: fields drawing minimap *)
}
  (*s: type settings *)
   and settings = {
     mutable draw_summary: bool;
     mutable draw_searched_rectangles: bool;
   }
  (*e: type settings *)
  and pos_and_line = {
    pos_to_line: Cairo.point -> int;
    line_to_pos: int -> Cairo.point;
  }
(*e: type drawing *)

(*s: new_pixmap() *)
let new_pixmap ~width ~height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE;
  drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true () ;
  drawable
(*e: new_pixmap() *)

(*s: init_drawing() *)
let init_drawing 
  (* This is a first guess. The first configure ev will force a resize. *)
  ?(width = 600)
  ?(height = 600)
  ?(width_minimap = 60)
  ?(height_minimap = 60)
  func 
  model
  layers
  paths
  root
 =

  let paths = paths +> List.map Common2.relative_to_absolute in
  let current_root = Common2.common_prefix_of_files_or_dirs paths in
  pr2_gen root;

  let treemap = 
    Common.profile_code "Visual.building the treemap" (fun () -> func paths) in
  let pm = new_pixmap ~width ~height in
  let readable_file_to_rect =
    treemap +> Common.map_filter (fun rect ->
      if not rect.T.tr_is_node
      then 
        let file  = rect.T.tr_label in
        let readable = Common.filename_without_leading_path root file in
        Some (readable, rect)
      else None
    ) +> Common.hash_of_list
  in

  {
    treemap = treemap;
    nb_rects = List.length treemap;
    readable_file_to_rect;
    current_root;
    treemap_func = func;

    dw_model = model;
    layers = layers;

    pos_and_line = Hashtbl.create 0;

    current_query = "";
    current_searched_rectangles = [];
    current_entity = None;
    current_grep_query = Hashtbl.create 0;

    pm = pm;
    overlay = Cairo.surface_create_similar (CairoH.surface_of_pixmap pm) 
      Cairo.CONTENT_COLOR_ALPHA width height;
    width = width;
    height = height;

    dw_settings = {
      (* todo: too fuzzy for now *)
      draw_summary = false;

      draw_searched_rectangles = true;
    };


    zoom = 1.;
    xtrans = 0.;
    ytrans = 0.;

    drag_pt = { Cairo.x = 0.0; Cairo.y = 0.0 };
    in_dragging = false;
    in_zoom_incruste = false;

    width_minimap = width_minimap;
    height_minimap = height_minimap;
    pm_minimap = new_pixmap ~width:width_minimap ~height:width_minimap;
    drag_pt_minimap = { Cairo.x = 0.0; Cairo.y = 0.0 };

  }
(*e: init_drawing() *)

(*****************************************************************************)
(* The drawing context *)
(*****************************************************************************)

(*s: type context *)
(* a slice of drawing used in the drawing functions *)
type context = {
  model: model Async.t;
  settings:settings;
  nb_rects_on_screen: int;
  grep_query: (Common.filename, int) Hashtbl.t;
  layers_microlevel: 
   (Common.filename, (int, Simple_color.emacs_color) Hashtbl.t) Hashtbl.t;
}
(*e: type context *)

let context_of_drawing dw = { 
  nb_rects_on_screen = dw.nb_rects;
  model = dw.dw_model;
  settings = dw.dw_settings;
  grep_query = dw.current_grep_query;
  layers_microlevel = dw.layers.Layer_code.micro_index;
}

(*****************************************************************************)
(* Point -> treemap info *)
(*****************************************************************************)

(*s: find_rectangle_at_user_point() *)
(* alt: could use Cairo_bigarray and the pixel trick if
 * it takes too long to detect which rectangle is under the cursor.
 * coud also sort the rectangles ... or have some kind of BSP.
 *)
let find_rectangle_at_user_point2 dw user =
  let user = CairoH.cairo_point_to_point user in

  let rects = dw.treemap in
  if List.length rects = 1
  then 
    (* we are fully zommed, this treemap will have tr_depth = 1 but we return
     * it *)
    let x = List.hd rects in
    Some (x, [], x)
  else 
   let matching_rects = rects 
    +> List.filter (fun r -> 
      F.point_is_in_rectangle user r.T.tr_rect
      && r.T.tr_depth > 1
    ) 
    +> List.map (fun r -> r, r.T.tr_depth) 
    +> Common.sort_by_val_highfirst 
    +> List.map fst
   in
   match matching_rects with
   | [] -> None
   | [x] -> Some (x, [], x)
   | _ -> Some (Common2.head_middle_tail matching_rects)

let find_rectangle_at_user_point a b = 
  Common.profile_code "Model.find_rectangle_at_point" (fun () ->
    find_rectangle_at_user_point2 a b)
(*e: find_rectangle_at_user_point() *)

(*e: model2.ml *)
