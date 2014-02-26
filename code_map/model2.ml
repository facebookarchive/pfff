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

module Flag = Flag_visual
module CairoH = Cairo_helpers
module F = Figures
module T = Treemap

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
  (*x: model fields hook *)
  big_grep_idx: Big_grep.index;
  (*e: model fields hook *)

  (* for microlevel use/def information *)
  g: Graph_code.graph option;
  (* fast accessors, for macrolevel use/def information  *)
  huses_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  husers_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  (* we used to store line information there, but the file may have changed *)
  hentities_of_file: (Common.filename, Graph_code.node list) Hashtbl.t;
 }
(*e: type model *)
type 'a deps = 'a list (* uses *) * 'a list (* users *)

(*****************************************************************************)
(* The drawing model *)
(*****************************************************************************)

type macrolevel = Treemap.treemap_rendering

(* 
 * We use different sources to provide fine-grained semantic visualization:
 * - the source code itself, lexed and parsed in parsing2.ml with language
 *   specific parsers, with the ASTs and tokens stored in a global cache
 * - a language agnostic 'glyph list array' computed from the AST and tokens
 *   by the language specific highlighter
 * - a language agnostic fuzzy defs identification based on the category of 
 *   the glyphs (but containing only "short nodes")
 * - the graph code computed for the whole project, usually not up to
 *   date with the most recent modifications, but containing useful
 *   global information such as the precise set of uses and users of an entity
 * - the light database (but could be replaced by the graph code)
 * 
 * We try to match specific glyphs to the right entity, then use
 * the graph code to find users (and uses) of this entity, and then going
 * from those entities to their corresponding glyph in this file or another
 * file in the whole treemap.
 *)

type microlevel = {
  point_to_line: Cairo.point -> line;
  line_to_rectangle: line -> Figures.rectangle;
  layout: layout;
  container: Treemap.treemap_rectangle;
  content: (glyph list) array option;
  (* sorted list of entities by line, defs based on highlighter *)
  defs: (line * short_node) list;
}
 (* 0-indexed line number, which is different from most tools, but
  * programs prefer 0-based index
  *)
  and line = Line of int
 (* Note that I don't use G.node because the string below is not fully
  * qualified so one must use match_short_vs_node when comparing with nodes.
  *)
  and short_node = (string * Database_code.entity_kind)
  and glyph = {
    str: string;
    categ: Highlight_code.category option;
    font_size: float;
    color: Simple_color.emacs_color;
    (* the lower left position, before calling Cairo.show_text str *)
    mutable pos: Cairo.point;
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
  model: model Async.t;
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

  settings: settings;

  (*s: fields drawing main view *)
  (* device coordinates *)
  (* first cairo layer, for heavy computation e.g. the treemap and content*)
  mutable base: [ `Any ] Cairo.surface;
  (* second cairo layer, when move the mouse *)
  mutable overlay: [ `Any ] Cairo.surface;
  (* todo? third cairo layer? for animations and time related graphics such
   * as tooltips, glowing rectangles, etc?
   *)
  (*e: fields drawing main view *)

  (*s: fields drawing viewport *)
  (* viewport, device coordinates *)
  mutable width: int;
  mutable height: int;
  (*e: fields drawing viewport *)

  (*s: fields drawing minimap *)
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


(*e: new_pixmap() *)

(*s: init_drawing() *)
let init_drawing 
  (* This is a first guess. The first configure ev will force a resize. *)
  ?(width = 600)
  ?(height = 600)
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
        let readable = Common.readable ~root file in
        Some (readable, rect)
      else None
    ) +> Common.hash_of_list
  in

  {
    treemap;
    nb_rects = List.length treemap;
    current_root;
    treemap_func = func;

    model; layers;

    readable_file_to_rect;
    microlevel = Hashtbl.create 0;

    current_query = "";
    current_searched_rectangles = [];
    current_entity = None;
    current_grep_query = Hashtbl.create 0;

    base = new_surface ~alpha:false ~width ~height;
    overlay = new_surface ~alpha:true ~width ~height;

    width; height;

    settings = {
      (* todo: too fuzzy for now *)
      draw_summary = false;
      draw_searched_rectangles = true;
    };
  }
(*e: init_drawing() *)

(*****************************************************************************)
(* The drawing context *)
(*****************************************************************************)

(*s: type context *)
(* a slice of drawing used in the drawing functions *)
type context = {
  model2: model Async.t;
  settings2:settings;
  nb_rects_on_screen: int;
  grep_query: (Common.filename, line) Hashtbl.t;
  layers_microlevel: 
   (Common.filename, (int, Simple_color.emacs_color) Hashtbl.t) Hashtbl.t;
}
(*e: type context *)

let context_of_drawing dw = { 
  nb_rects_on_screen = dw.nb_rects;
  model2 = dw.model;
  settings2 = dw.settings;
  grep_query = dw.current_grep_query;
  layers_microlevel = dw.layers.Layer_code.micro_index;
}

(*****************************************************************************)
(* Point -> (rectangle, line, glyph, entity) *)
(*****************************************************************************)

(*s: find_rectangle_at_user_point() *)
(* alt: we could use Cairo_bigarray and the pixel trick below if
 * it takes too long to detect which rectangle is under the cursor.
 * We could also sort the rectangles ... or have some kind of BSP.
 * Add in model:
 *   mutable pm_color_trick: GDraw.pixmap;
 *   mutable pm_color_trick_info: (string) array.
 * 
 * current solution: just find pixel by iterating over all the rectangles
 * and check if it's inside.
 *)
let find_rectangle_at_user_point2 user dw =
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
        F.point_is_in_rectangle user r.T.tr_rect && r.T.tr_depth > 1
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


let find_line_in_rectangle_at_user_point user r dw =
  try 
    let microlevel = Hashtbl.find dw.microlevel r in
    let line = microlevel.point_to_line user in
    Some line
  with Not_found -> None

let find_glyph_in_rectangle_at_user_point user r dw =
  find_line_in_rectangle_at_user_point user r dw >>= (fun line ->
    let microlevel = Hashtbl.find dw.microlevel r in
    microlevel.content >>= (fun glyphs ->
      let (Line line) = line in
      let glyphs = glyphs.(line) in
      (* find the best one *)
      glyphs +> List.rev +> Common.find_opt (fun glyph ->
        let pos = glyph.pos in
        user.Cairo.x >= pos.Cairo.x
      )
    )
  )

(*****************************************************************************)
(* Graph code integration *)
(*****************************************************************************)

let match_short_vs_node (str, kind) node =
  snd node =*= kind && 
  Graph_code.shortname_of_node node =$= str

(* We used to just look in hentities_of_file for the line mentioned
 * in the graph_code database, but the file may have changed so better
 * instead to rely on microlevel.defs.
 *)
let find_def_entity_at_line_opt line r dw =
  let model = Async.async_get dw.model in
  let file = r.T.tr_label in
  let readable = Common.readable ~root:model.root file in
  try 
    let nodes = Hashtbl.find model.hentities_of_file readable in
    let microlevel = Hashtbl.find dw.microlevel r in
    let short_node = List.assoc line microlevel.defs in
    (* try to match the possible shortname str with a fully qualified node *)
    nodes +> Common.find_some_opt (fun node ->
      if match_short_vs_node short_node node
      then Some node
      else None
    )
  with Not_found -> None


let deps_readable_files_of_file file dw =
  let model = Async.async_get dw.model in
  let readable = Common.readable ~root:model.root file in

  let uses = 
    try Hashtbl.find model.huses_of_file readable with Not_found -> [] in
  let users = 
    try Hashtbl.find model.husers_of_file readable with Not_found -> [] in
  uses, users

let deps_readable_files_of_node node dw =
  let model = Async.async_get dw.model in
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

let deps_rects_of_file file dw =
  let uses, users = deps_readable_files_of_file file dw in
  uses +> Common.map_filter (fun file -> 
    Common2.optionise (fun () -> Hashtbl.find dw.readable_file_to_rect file)
  ),
  users +> Common.map_filter (fun file ->
    Common2.optionise (fun () ->Hashtbl.find dw.readable_file_to_rect file)
  )


let uses_or_users_of_node node dw fsucc =
  let model = Async.async_get dw.model in
  match model.g with
  | None -> []
  | Some g ->
    let succ = fsucc node Graph_code.Use g in
    succ +> Common.map_filter (fun n ->
      try 
        let file = Graph_code.file_of_node n g in
        (* rectangles not on the screen will be automatically "clipped"
         * as this may raise Not_found 
         *)
        let rect = Hashtbl.find dw.readable_file_to_rect file in
        let microlevel = Hashtbl.find dw.microlevel rect in
        let line = microlevel.defs +> List.find (fun (_line, snode) ->
          match_short_vs_node snode n
        ) +> fst in
        Some (n, line, microlevel)
      with Not_found -> None
    )

let deps_of_node_clipped node dw =
  uses_or_users_of_node node dw Graph_code.succ,
  uses_or_users_of_node node dw Graph_code.pred


let lines_where_used_node node startl microlevel =
  let s = Graph_code.shortname_of_node node in
  let s =
    (* ugly: see Graph_code_clang.new_str_if_defs() where we rename dupes *)
    match s with
    | _ when (s =~ "\\(.*\\)__[0-9]+$") -> Common.matched1 s
    | _ when (s =~ "^\\$\\(.*\\)") -> Common.matched1 s
    | _ -> s
  in
  
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
        Database_code.matching_use_categ_kind categ (snd node)
      )
      then Common.push (Line line) res
    done;
    !res

(*e: model2.ml *)
