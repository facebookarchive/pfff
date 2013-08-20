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

module Flag = Flag_visual

(*****************************************************************************)
(* The code model *)
(*****************************************************************************)

(* 0-indexed line number, which is different from most tools, but
 * programs prefer 0-based index
 *)
type line = Line of int

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

  (* for microlevel use/def information *)
  g: Graph_code.graph option;
  (* fast accessors, for macrolevel use/def information  *)
  huses_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  husers_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  (* the lists are sorted by line number *)
  hentities_of_file: 
    (Common.filename, (line * Graph_code.node) list)  Hashtbl.t;
 }
(*e: type model *)


(*****************************************************************************)
(* The drawing model *)
(*****************************************************************************)

type macrolevel = Treemap.treemap_rendering

type microlevel = {
  pos_to_line: Cairo.point -> line;
  line_to_rectangle: line -> Figures.rectangle;
  layout: layout;
  container: Treemap.treemap_rectangle;
  content: (glyph list) array option;
}
  and glyph = {
    str: string;
    categ: Highlight_code.category option;
    font_size: float;
    color: Simple_color.emacs_color;
  }
  and layout = {
    lfont_size: float;
    split_nb_columns: float; (* int *)
    width_per_column:float;
    height_per_line: float;
    nblines: float; (* int *)
    nblines_per_column: float; (* int *)
  }

(*s: type drawing *)
(* All the 'float' below are to be intepreted as user coordinates except when
 * explicitely mentioned. All the 'int' are usually device coordinates.
 *)
type drawing = {

  (* computed lazily, semantic information about the code *)
  dw_model: model Async.t;
  (* to compute a new treemap based on user's action *)
  treemap_func: Common.path list -> Treemap.treemap_rendering;

  (* Macrolevel. In user coordinates from 0 to T.xy_ratio for 'x' and 0 to 1
   * for 'y'. Assumes the treemap contains absolute paths (tr.tr_label).
   *)
  treemap: Treemap.treemap_rendering;
  (* Microlevel. When we render content at the microlevel, we then need to
   * know to which line corresponds a position and vice versa.
   *)
  microlevel: (Treemap.treemap_rectangle, microlevel) Hashtbl.t;

  (* generated from dw.treemap, contains readable path relative to model.root *)
  readable_file_to_rect: 
    (Common.filename, Treemap.treemap_rectangle) Hashtbl.t;
  (* coupling: = List.length treemap *)
  nb_rects: int; 
  (* This is to display readable paths. When fully zoomed it's a filename *)
  current_root: Common.path;

  mutable layers: Layer_code.layers_with_index;

  (*s: fields drawing query stuff *)
    (* queries *)
    mutable current_query: string;
    mutable current_searched_rectangles: Treemap.treemap_rectangle list;
    mutable current_entity: Database_code.entity option;
    mutable current_grep_query: (Common.filename, line) Hashtbl.t;
  (*e: fields drawing query stuff *)

  dw_settings: settings;

  (*s: fields drawing main view *)
    (* device coordinates *)
    mutable pm: GDraw.pixmap;
    mutable overlay: [ `Any ] Cairo.surface;

  (*e: fields drawing main view *)

  (*s: fields drawing viewport *)
    (* viewport, device coordinates *)
    mutable width: int;
    mutable height: int;

    (* to delete? *)
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

  let paths = List.map Common2.relative_to_absolute paths in
  let current_root = Common2.common_prefix_of_files_or_dirs paths in
  let treemap = 
    Common.profile_code "Visual.building the treemap" (fun () -> func paths) in
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
  let pm = new_pixmap ~width ~height in

  {
    treemap;
    nb_rects = List.length treemap;
    current_root;
    treemap_func = func;

    dw_model = model;
    layers = layers;

    readable_file_to_rect;
    microlevel = Hashtbl.create 0;

    current_query = "";
    current_searched_rectangles = [];
    current_entity = None;
    current_grep_query = Hashtbl.create 0;

    pm;
    overlay = Cairo.surface_create_similar (CairoH.surface_of_pixmap pm) 
      Cairo.CONTENT_COLOR_ALPHA width height;
    width;
    height;

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
  grep_query: (Common.filename, line) Hashtbl.t;
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
(* Point -> (rectangle, line, entity) *)
(*****************************************************************************)

(*s: find_rectangle_at_user_point() *)
(* alt: could use Cairo_bigarray and the pixel trick below if
 * it takes too long to detect which rectangle is under the cursor.
 * coud also sort the rectangles ... or have some kind of BSP.
 * 
 * going from a point to the enclosing rectangle via pixel color trick. 
 * Kind of ugly. Add this in the model:
 *   mutable pm_color_trick: GDraw.pixmap;
 *   mutable pm_color_trick_info: (string) array.
 * 
 * Current solution: just find pixel by iterating over all the rectangles
 * and check if he's inside.
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
    (* opti: this should be far faster by using a quad tree to represent
     * the treemap
     *)
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


let find_line_in_rectangle_at_user_point dw user_pt r =
  try 
    let microlevel = Hashtbl.find dw.microlevel r in
    let line = microlevel.pos_to_line user_pt in
    Some line
  with Not_found -> None

(*****************************************************************************)
(* Graph code integration *)
(*****************************************************************************)

let find_entity_at_line line r dw =
  let model = Async.async_get dw.dw_model in
  let file = r.T.tr_label in
  let readable = Common.filename_without_leading_path model.root file in
  try 
    let xs = Hashtbl.find model.hentities_of_file readable in
    xs +> List.rev +> Common.find_some_opt (fun (line2, n) ->
      if line = line2 (* && abs (line - line2) <= 4 *)
      then Some n 
      else None
    )
  with Not_found -> None


let uses_and_users_readable_files_of_file file dw =
  let model = Async.async_get dw.dw_model in
  let readable = Common.filename_without_leading_path model.root file in

  let uses = 
    try Hashtbl.find model.huses_of_file readable with Not_found -> [] in
  let users = 
    try Hashtbl.find model.husers_of_file readable with Not_found -> [] in
  uses, users

let uses_and_users_readable_files_of_node node dw =
  let model = Async.async_get dw.dw_model in
  match model.g with
  | None -> [], []
  | Some g ->
    let succ = Graph_code.succ node Graph_code.Use g in
    let pred = Graph_code.pred node Graph_code.Use g in
    succ +> Common.map_filter (fun n ->
      try Some (Graph_code.file_of_node n g) with Not_found -> None
    ),
    pred +> Common.map_filter (fun n ->
      try Some (Graph_code.file_of_node n g) with Not_found -> None
    )

let uses_and_users_rect_of_file file dw =
  let uses, users = uses_and_users_readable_files_of_file file dw in
  uses +> Common.map_filter (fun file -> 
    Common2.optionise (fun () -> Hashtbl.find dw.readable_file_to_rect file)
  ),
  users +> Common.map_filter (fun file ->
    Common2.optionise (fun () ->Hashtbl.find dw.readable_file_to_rect file)
  )


let uses_or_users_of_node node dw fsucc =
  let model = Async.async_get dw.dw_model in
  match model.g with
  | None -> []
  | Some g ->
    let succ = fsucc node g in
    succ +> Common.map_filter (fun n ->
      try 
        let file = Graph_code.file_of_node n g in
        let rect = Hashtbl.find dw.readable_file_to_rect file in
        let xs = Hashtbl.find model.hentities_of_file file in
        let (line, _n2) = xs +> List.find (fun (_, n2) -> n2 =*= n) in
        let microlevel = Hashtbl.find dw.microlevel rect in
        Some (n, line, microlevel)
      with Not_found -> None
    )

let uses_and_users_of_node node dw =
  uses_or_users_of_node node dw (fun node g ->
    Graph_code.succ node Graph_code.Use g),
  uses_or_users_of_node node dw (fun node g ->
    Graph_code.pred node Graph_code.Use g)


let lines_where_used_node node startl microlevel =
  let (fullstr, kind) = node in
  let xs = Common.split "\\." fullstr in
  let s = Common2.list_last xs in
  
  let (Line startl) = startl in
  match microlevel.content with
  | None -> []
  | Some glypys ->
    
    let res = ref [] in
    for line = startl to Array.length glypys - 1 do
      let xs = glypys.(line) in
      if xs +> List.exists (fun glyph ->
        let categ =
          match glyph.categ with
          | Some x -> x
          | _ -> Highlight_code.Normal
        in
        glyph.str =$= s &&
          
        (* see the code of the different highlight_code_xxx.ml to
         * know the different possible pairs
         *)
        (match kind, categ with
        | Database_code.Function, Highlight_code.Function _
        | Database_code.Field, Highlight_code.Field _
        | Database_code.Constructor, Highlight_code.ConstructorUse _
        | Database_code.Constructor, Highlight_code.ConstructorMatch _
        | Database_code.Global, Highlight_code.Global _

        (* tofix at some point, wrong tokenizer *)
        | Database_code.Constant, Highlight_code.Local _
        | Database_code.Global, Highlight_code.Local _
        | Database_code.Function, Highlight_code.Local _

        | Database_code.Global, Highlight_code.UseOfRef
         -> true

        | _ -> false
        )
      )
      then Common.push2 (Line line) res
    done;
    !res

(*e: model2.ml *)
