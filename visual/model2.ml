(*s: model2.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
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
  db: Database_code.database option;
 
  (*s: model fields hook *)
    (* fast accessors *)
    hentities : (string, Database_code.entity) Hashtbl.t;
  (*x: model fields hook *)
    hfiles_entities : (Common.filename, Database_code.entity list) Hashtbl.t;
  (*x: model fields hook *)
    big_grep_idx: Big_grep.index;
  (*e: model fields hook *)
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
  
  (* to compute zoomed treemap when double click *)
  treemap_func: Common.path list -> Treemap.treemap_rendering;

  (* This is to display readable paths. When fully zoomed it's a filename *)
  root: Common.path;

  (* computed lazily *)
  dw_model: model Async.t;

  (*s: fields drawing query stuff *)
    (* queries *)
    mutable current_query: string;
    mutable current_searched_rectangles: Treemap.treemap_rectangle list;
    mutable current_entity: Database_code.entity option;
    mutable current_grep_query : 
      (Common.filename, int) Hashtbl.t;
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
(*e: type drawing *)

(*s: new_pixmap() *)
let new_pixmap ~width ~height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE ;
  drawable#rectangle
    ~x:0 ~y:0 ~width ~height ~filled:true () ;
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
  paths
 =

  let paths = paths +> Common.map Common.relative_to_absolute in
  let root = Common.common_prefix_of_files_or_dirs paths in
  pr2_gen root;

  let treemap = 
   Common.profile_code2 "Visual.building the treemap" (fun () -> func paths) in
  let pm = new_pixmap ~width ~height in

  {
    treemap = treemap;
    nb_rects = List.length treemap;
    root = root;
    treemap_func = func;

    dw_model = model;

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
}
(*e: type context *)

let context_of_drawing dw = { 
  nb_rects_on_screen = dw.nb_rects;
  model = dw.dw_model;
  settings = dw.dw_settings;
  grep_query = dw.current_grep_query;
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
   | _ -> Some (Common.head_middle_tail matching_rects)


let find_rectangle_at_user_point a b = 
  Common.profile_code "Model.find_rectangle_at_point" (fun () ->
    find_rectangle_at_user_point2 a b)
(*e: find_rectangle_at_user_point() *)

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

(*s: readable_to_absolute_filename_under_root *)
(* People may run the visualizer on a subdir of what is mentionned in the
 * database code (e.g. subdir ~/www/flib of ~/www). The light_db
 * contains only readable paths (e.g. flib/foo.php); the reason for
 * those readable paths is that we want to reuse the light_db
 * and share it among multiple users which may have
 * different paths for their own software repo (e.g. ~/www4/).
 * 
 * When the user select an entity through the search box,
 * we will know the readable paths of the entity he is looking for
 * but we need a full path for refreshing the treemap.
 * We can not just concatenate the root with the readable paths which
 * in the example would lead to the path  ~/www/flib/flib/foo.php.
 * 
 * The goal of the function below is given a readable path like
 * flib/foo.php and a root like ~/www/flib  to recognize the common part
 * and return a valid fullpath like ~/www/flib/foo.php
 * 
 *)
let rec readable_to_absolute_filename_under_root ~root filename =

  (* the root may be a filename *)
  let root_dir = 
    if is_directory root then root
    else Filename.dirname root
  in

  let root_and_parents =
    Common.inits_of_absolute_dir root_dir +> List.rev
  in
  try 
    root_and_parents +> Common.return_when (fun dir ->
      let path = Filename.concat dir filename in
      if Sys.file_exists path
      then Some path
      else None
    )
  with Not_found ->
    failwith 
      (spf "can't find file %s with root = %s" filename root)
(*e: readable_to_absolute_filename_under_root *)

(*s: actual_root_of_db *)
let actual_root_of_db ~root db =
  let a_file = (db.Db.entities.(0)).Db.e_file in
  let absolute_file = 
    readable_to_absolute_filename_under_root root a_file in
  
  if absolute_file =~ ("\\(.*\\)/" ^ a_file)
  then Common.matched1 absolute_file
  else failwith (spf "Could not find actual_root of %s under %s: "
                    absolute_file root)
(*e: actual_root_of_db *)

(*****************************************************************************)
(* Entities info *)
(*****************************************************************************)

(*s: hentities() *)
(* We want to display very often used functions in bigger size font.
 * Enter database_code.ml which provides a language-independent database of
 * information on source code.
 * 
 * We compute the entities outside init_drawing because
 * init_drawing can be called multiple times (when we zoom in)
 * and we dont want the heavy entities computation to be 
 * repeated.
 *)
let hentities root db_opt = 
  let hentities = Hashtbl.create 1001 in

  db_opt +> Common.do_option (fun db ->

    let actual_root = actual_root_of_db ~root db in

      (* todo sanity check that db talks about files
       * in dirs_or_files ? Ensure same readable path.
       *)
      db.Db.entities +> Array.iter (fun e ->
        Hashtbl.add hentities
          e.Db.e_name
          {e with Db.e_file = 
              Filename.concat actual_root e.Db.e_file
          }
      );
  );
  hentities
(*e: hentities() *)

(*s: hfiles_and_top_entities() *)
(* used in the summary mixed mode *)
let hfiles_and_top_entities root db_opt =
  let hfiles = Hashtbl.create 1001 in

  db_opt +> Common.do_option (fun db ->
    let ksorted = 
      Db.build_top_k_sorted_entities_per_file ~k:5 db.Db.entities in
    let actual_root = actual_root_of_db ~root db in

    Hashtbl.iter (fun k v ->
      let k' = Filename.concat actual_root k in
      Hashtbl.add hfiles k' v
    ) ksorted
  );
  hfiles
(*e: hfiles_and_top_entities() *)

(*****************************************************************************)
(* Completion data *)
(*****************************************************************************)

(*s: all_entities *)
(* We want to provide completion not only for functions/class/methods
 * but also for files and directory themselves.
 * 
 * We pass the root in addition to the db_opt because sometimes we 
 * don't have a db but we still want to provide completion for the 
 * dirs and files.
 * 
 * todo: what do do when the root of the db is not the root
 * of the treemap ?
 *)
let all_entities db_opt root =
  match db_opt with
  | None -> 
      let db = Database_code.files_and_dirs_database_from_root root in
      Database_code.files_and_dirs_and_sorted_entities_for_completion
        ~threshold_too_many_entities:!Flag.threshold_too_many_entities
        db

  | Some db ->
      let nb_entities = Array.length db.Db.entities in
      let nb_files = List.length db.Db.files in
      pr2 (spf "We got %d entities in %d files" nb_entities nb_files);

      Database_code.files_and_dirs_and_sorted_entities_for_completion
        ~threshold_too_many_entities:!Flag.threshold_too_many_entities
        db
(*e: all_entities *)
(*e: model2.ml *)
