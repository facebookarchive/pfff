(*s: model2.mli *)

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

  mutable layers: Layer_code.layers_with_index;

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
val context_of_drawing: drawing -> context

(*s: init_drawing sig *)
val init_drawing :
  ?width:int ->
  ?height:int ->
  ?width_minimap:int ->
  ?height_minimap:int ->
  (Common.path list -> Treemap.treemap_rendering) ->
  model Async.t ->
  Layer_code.layers_with_index ->
  Common.filename list -> 
  drawing
(*e: init_drawing sig *)

(*s: new_pixmap sig *)
val new_pixmap : 
  width:int -> height:int -> GDraw.pixmap
(*e: new_pixmap sig *)

(*s: find_rectangle_at_user_point sig *)
val find_rectangle_at_user_point :
  drawing ->
  Cairo.point ->
  (Treemap.treemap_rectangle * 
   Treemap.treemap_rectangle list * 
   Treemap.treemap_rectangle
  )
  option
(*e: find_rectangle_at_user_point sig *)


(*s: hentities sig *)
val hentities :
  Common.path -> Database_code.database option -> 
  (string, Database_code.entity) Hashtbl.t
(*e: hentities sig *)

(*s: hfiles_and_top_entities sig *)
val hfiles_and_top_entities :
  Common.path -> Database_code.database option -> 
  (Common.filename, Database_code.entity list) Hashtbl.t
(*e: hfiles_and_top_entities sig *)

(*s: all_entities sig *)
(* Will generate extra entities for files, dirs, and also generate
 * an extra entity when have a fullname that is not empty
 *)
val all_entities :
  Database_code.database option -> Common.dirname ->
  Database_code.entity list
(*e: all_entities sig *)


(*s: readable_to_absolute_filename_under_root sig *)
val readable_to_absolute_filename_under_root :
  root:Common.path -> string -> string
(*e: readable_to_absolute_filename_under_root sig *)

(*s: actual_root_of_db sig *)
val actual_root_of_db : 
  root:Common.path -> Database_code.database -> string
(*e: actual_root_of_db sig *)

(*e: model2.mli *)
