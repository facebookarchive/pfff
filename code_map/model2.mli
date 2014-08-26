(*s: model2.mli *)

(*s: type model *)
(* filename below should be in readable path format *)
type model = {
  (* for translating the absolute filenames in tr_label in readable so
   * one can access the node in the model for a tr_rectangle 
   *)
  root: Common.dirname; 

  db: Database_code.database option;
  (*s: model fields hook *)
  (* fast accessors *)
  hentities : (string (* short name *), Database_code.entity) Hashtbl.t;
  (*x: model fields hook *)
  (*x: model fields hook *)
  big_grep_idx: Big_grep.index;
  (*e: model fields hook *)

  (* for microlevel use/def information *)
  g: Graph_code.graph option;
  (* for macrolevel use/def information, only for Dir and File *)
  hfile_deps_of_node: (Graph_code.node, Common.filename deps) Hashtbl.t;
  (* we used to store line information there, but the file may have changed *)
  hentities_of_file: (Common.filename, Graph_code.node list) Hashtbl.t;
 }
(*e: type model *)
and 'a deps = 'a list (* uses *) * 'a list (* users *)


type macrolevel = Treemap.treemap_rendering



type microlevel = {
  point_to_line: Cairo.point -> line;
  line_to_rectangle: line -> Figures.rectangle;
  layout: layout;
  container: Treemap.treemap_rectangle;
  (* the lines of the files, 0-based indexed line, see line type below *)
  content: (glyph list) array option;
  (* defs based on highlighters categories *)
  defs: (line * short_node) list;
}
  (* 0-indexed line number, which is different from most tools, but
   * programs prefer 0-based index
   *)
  and line = Line of int

  and layout = {
    lfont_size: float;
    split_nb_columns: float; (* int *)
    width_per_column:float;
    height_per_line: float;
    nblines: float; (* int *)
    nblines_per_column: float; (* int *)
  }

  and glyph = {
    str: string;
    categ: Highlight_code.category option;
    font_size: float;
    color: Simple_color.emacs_color;
    mutable pos: Cairo.point;
  }

 (* Note that I don't use G.node because the string below is not fully
  * qualified so one must use match_short_vs_node when comparing with nodes.
  *)
  and short_node = (string * Entity_code.entity_kind)

(*s: type drawing *)
(* All the 'float' below are to be intepreted as user coordinates except when
 * explicitely mentioned. All the 'int' are usually device coordinates.
 *)
type drawing = {

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
  (* Used to display readable paths. When fully zoomed it's a filename. *)
  current_root: Common.path;

  mutable layers: Layer_code.layers_with_index;

  (*s: fields drawing query stuff *)
  (* queries *)
  mutable current_query: string;
  mutable current_searched_rectangles: Treemap.treemap_rectangle list;
  mutable current_grep_query: (Common.filename, line) Hashtbl.t;
  (*e: fields drawing query stuff *)

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
  (*e: type settings *)
(*e: type drawing *)

type world = {
  mutable dw: drawing;
  dw_stack: drawing Common.stack ref;

  (* computed lazily, semantic information about the code *)
  model: model Async.t;
  root_orig: Common.dirname;
  (* to compute a new treemap based on user's action *)
  treemap_func: Common.path list -> Treemap.treemap_rendering;
  (* misc settings, not really used for now *)
  settings: settings;

  mutable current_node: Graph_code.node option;
  mutable current_node_selected: Graph_code.node option;
  mutable current_entity: Database_code.entity option;
}
   and settings = {
     mutable draw_summary: bool;
     mutable draw_searched_rectangles: bool;
   }

(*s: type context *)
(* a slice of drawing used in the drawing functions *)
type context = {
  model2: model Async.t;
  nb_rects_on_screen: int;
  grep_query: (Common.filename, line) Hashtbl.t;
  layers_microlevel: 
   (Common.filename, (int, Simple_color.emacs_color) Hashtbl.t) Hashtbl.t;
}
(*e: type context *)
val context_of_drawing: drawing -> model Async.t -> context


(*s: init_drawing sig *)
val init_drawing :
  ?width:int ->
  ?height:int ->
  (Common.path list -> Treemap.treemap_rendering) ->
  Layer_code.layers_with_index ->
  Common.path list -> 
  Common.dirname (* root *) ->
  drawing
(*e: init_drawing sig *)

(*s: new_pixmap sig *)
val new_surface: 
  alpha:bool -> width:int -> height:int -> [ `Any ] Cairo.surface
(*e: new_pixmap sig *)

(* point -> rectangle -> line -> glyph -> entity *)

(*s: find_rectangle_at_user_point sig *)
val find_rectangle_at_user_point :
  Cairo.point -> drawing ->
  (Treemap.treemap_rectangle * (* most precise *)
   Treemap.treemap_rectangle list * (* englobbing ones *)
   Treemap.treemap_rectangle (* top one *)
  ) option
(*e: find_rectangle_at_user_point sig *)

val find_line_in_rectangle_at_user_point:
  Cairo.point -> Treemap.treemap_rectangle -> drawing -> line option
val find_glyph_in_rectangle_at_user_point:
  Cairo.point -> Treemap.treemap_rectangle -> drawing -> glyph option

(* graph code integration *)

val find_def_entity_at_line_opt:
  line -> Treemap.treemap_rectangle -> drawing -> model -> 
  Graph_code.node option
val find_use_entity_at_line_and_glyph_opt:
  line -> glyph -> Treemap.treemap_rectangle -> drawing -> model -> 
  Graph_code.node option

(* macrolevel deps *)
val node_of_rect: 
  Treemap.treemap_rectangle -> model -> Graph_code.node

val deps_readable_files_of_node:
  Graph_code.node -> model -> 
  Common.filename (* readable *) deps

val deps_rects_of_rect: 
  Treemap.treemap_rectangle -> drawing -> model ->
  Treemap.treemap_rectangle deps

(* microlevel deps *)
val deps_nodes_of_node_clipped:
  Graph_code.node -> drawing -> model ->
  (Graph_code.node * line * microlevel) deps

(* line highlight *)
val line_and_microlevel_of_node_opt:
  Graph_code.node -> drawing -> model -> 
  (Graph_code.node * line * microlevel) option

val lines_where_used_node:
  Graph_code.node -> line -> microlevel -> line list

(*e: model2.mli *)
