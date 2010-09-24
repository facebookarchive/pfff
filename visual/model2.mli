
type 'a async = {
  m: Mutex.t; 
  c: Condition.t;
  v: 'a option ref;
  }
val async_get: 'a async -> 'a
val async_make: unit -> 'a async
val async_set: 'a -> 'a async -> unit

val locked: (unit -> 'a) -> Mutex.t -> 'a

type model = {
  db: Database_code.database option;
  hentities : (string, Database_code.entity) Hashtbl.t;
  hfiles_entities : (Common.filename, Database_code.entity list) Hashtbl.t;
  big_grep_idx: Big_grep.index;
 }

type drawing = {
  treemap : Treemap.treemap_rendering;
  nb_rects: int; (* coupling: = List.length treemap *)

  treemap_func : Common.path list -> Treemap.treemap_rendering;
  (* can be a file *)
  root : Common.path;

  model: model async;

  mutable current_query : string;
  mutable current_searched_rectangles: Treemap.treemap_rectangle list;
  mutable current_entity: Database_code.entity option;
  mutable current_grep_query : 
    (Common.filename, int) Hashtbl.t;

  settings: settings;

  mutable pm : GDraw.pixmap;
  mutable overlay : [ `Any ] Cairo.surface;

  mutable width : int;
  mutable height : int;
  mutable zoom : float;
  mutable xtrans : float;
  mutable ytrans : float;

  mutable drag_pt : Cairo.point;
  mutable in_dragging : bool;

  mutable in_zoom_incruste : bool;

  mutable pm_minimap : GDraw.pixmap;
  mutable width_minimap : int;
  mutable height_minimap : int;
  mutable drag_pt_minimap : Cairo.point;
}
 and settings = {
   mutable draw_summary: bool;
   mutable draw_searched_rectangles: bool;
 }

val init_drawing :
  ?width:int ->
  ?height:int ->
  ?width_minimap:int ->
  ?height_minimap:int ->
  (Common.path list -> Treemap.treemap_rendering) ->
  model async ->
  Common.filename list -> 
  drawing

val new_pixmap : 
  width:int -> height:int -> GDraw.pixmap



val find_rectangle_at_user_point :
  drawing ->
  Cairo.point ->
  (Treemap.treemap_rectangle * 
   Treemap.treemap_rectangle list * 
   Treemap.treemap_rectangle
  )
  option

val hentities :
  Common.path -> Database_code.database option -> 
  (string, Database_code.entity) Hashtbl.t

val hfiles_and_top_entities :
  Common.path -> Database_code.database option -> 
  (string, Database_code.entity list) Hashtbl.t

(* Will generate extra entities for files, dirs, and also generate
 * an extra entity when have a fullname that is not empty
 *)
val all_entities :
  Database_code.database option ->
  Database_code.entity list


val readable_to_absolute_filename_under_root :
  root:Common.path -> string -> string
val actual_root_of_db : 
  root:Common.path -> Database_code.database -> string
