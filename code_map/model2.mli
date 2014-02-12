(*s: model2.mli *)

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
  (* todo: what about huses_of_dir? scalable spirit, should have deps
   * of dirs, files, entities, even locals
   *)
  huses_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  husers_of_file: (Common.filename, Common.filename list) Hashtbl.t;
  (* the lists are sorted by line number *)
  hentities_of_file: 
    (Common.filename, (line * Graph_code.node) list)  Hashtbl.t;
 }
(*e: type model *)
type 'a deps = 'a list (* uses *) * 'a list (* users *)


type macrolevel = Treemap.treemap_rendering

type microlevel = {
  pos_to_line: Cairo.point -> line;
  line_to_rectangle: line -> Figures.rectangle;
  layout: layout;
  container: Treemap.treemap_rectangle;
  (* the lines of the files, 0-based indexed line, see line type below *)
  content: (glyph list) array option;
  defs: (line * (string * Database_code.entity_kind)) list;
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
  Common.dirname ->
  drawing
(*e: init_drawing sig *)

(*s: new_pixmap sig *)
val new_pixmap : 
  width:int -> height:int -> GDraw.pixmap
(*e: new_pixmap sig *)

(* point -> rectangle -> line -> entity *)

(*s: find_rectangle_at_user_point sig *)
val find_rectangle_at_user_point :
  drawing ->
  Cairo.point ->
  (Treemap.treemap_rectangle * (* most precise *)
   Treemap.treemap_rectangle list * (* englobbing ones *)
   Treemap.treemap_rectangle (* top one *)
  ) option
(*e: find_rectangle_at_user_point sig *)

val find_line_in_rectangle_at_user_point:
  drawing ->
  Cairo.point -> 
  Treemap.treemap_rectangle ->
  line option

val find_def_entity_at_line_opt:
  line -> Treemap.treemap_rectangle -> drawing -> 
  Graph_code.node option


(* graph code integration *)

(* macrolevel uses and users *)
val deps_readable_files_of_file:
  Common.filename (* abs *) -> drawing -> Common.filename (* readable *) deps

val deps_rect_of_file: 
  Common.filename -> drawing -> Treemap.treemap_rectangle deps

(* microlevel uses and users *)
val deps_readable_files_of_node:
  Graph_code.node -> drawing -> Common.filename (* readable *) deps

val deps_of_node:
  Graph_code.node -> drawing -> (Graph_code.node * line * microlevel) deps

val lines_where_used_node:
  Graph_code.node -> line -> microlevel -> line list

(*e: model2.mli *)
