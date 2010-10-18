(*s: treemap.mli *)

open Figures

(*s: type treemap *)
type ('dir, 'file) treemap = 
 (treemap_rect * 'dir, treemap_rect * 'file) Common.tree
    and treemap_rect = { 
      size : int; 
      color : Simple_color.color; 
      label: string;
    }
(*e: type treemap *)

val xy_ratio : float

val rect_ortho: rectangle

type treemap_rendering = treemap_rectangle list
 and treemap_rectangle = {
   tr_rect: rectangle;
   tr_color: int (* Simple_color.color *);
   tr_label: string;
   tr_depth: int;
   tr_is_node: bool;
 }

(*s: type screen_dim *)
type screen_dim = {
  (* total width/height *)
  w: int;
  h: int;
  (* the viewport *)
  w_view: int;
  h_view: int;
  (* extra information *)
  h_status: int;
  w_legend: int;
}
(*e: type screen_dim *)

(*s: type algorithm *)
type algorithm = 
  | Classic
  | Squarified
  | SquarifiedNoSort
  | Ordered of pivot

  and pivot = 
    | PivotBySize
    | PivotByMiddle
(*e: type algorithm *)

(*s: type layout_func *)
type ('a, 'b) layout_func = 
  (float * ('a, 'b) treemap) list ->
  int ->
  rectangle ->
  (float * ('a, 'b) treemap * rectangle) list
(*e: type layout_func *)

(*s: signature algos *)
val algos: algorithm list

val layoutf_of_algo: algorithm -> ('a, 'b) layout_func

(*e: signature algos *)

val render_treemap_algo: 
  ?algo:algorithm -> 
  ?big_borders:bool ->
  ('dir, 'file) treemap -> treemap_rendering




(* treemap maker, see also treemap_json.ml *)
(*s: signature treemap_of_tree *)
val treemap_of_tree :
  size_of_leaf:('file -> int) ->
  color_of_leaf:('file -> Simple_color.color) ->
  ?label_of_file:('file -> string) ->
  ?label_of_dir:('dir -> string) ->
  ('dir, 'file) Common.tree ->
  ('dir, 'file) treemap
(*e: signature treemap_of_tree *)

(* tree maker, see also Common.tree2_of_files *)
(*s: signature tree_of_dir *)
type directory_sort = 
  | NoSort
  | SortDirThenFiles
  | SortDirAndFiles
  | SortDirAndFilesCaseInsensitive

val tree_of_dir:
  ?filter_file:(Common.filename -> bool) ->
  ?filter_dir:(Common.dirname -> bool) ->
  ?sort:directory_sort ->
  file_hook:(Common.filename -> 'a) -> 
  Common.dirname -> 
  (Common.dirname, Common.filename * 'a) Common.tree
(*e: signature tree_of_dir *)

val tree_of_dir_or_file:
  ?filter_file:(Common.filename -> bool) ->
  ?filter_dir:(Common.dirname -> bool) ->
  ?sort:directory_sort ->
  file_hook:(Common.filename -> 'a) -> 
  Common.path -> 
  (Common.dirname, Common.filename * 'a) Common.tree

val tree_of_dirs_or_files:
  ?filter_file:(Common.filename -> bool) ->
  ?filter_dir:(Common.dirname -> bool) ->
  ?sort:directory_sort ->
  file_hook:(Common.filename -> 'a) -> 
  Common.path list -> 
  (Common.dirname, Common.filename * 'a) Common.tree

val remove_singleton_subdirs:
  (Common.dirname, Common.filename * 'a) Common.tree ->
  (Common.dirname, Common.filename * 'a) Common.tree


(* internal functions *)
(*s: signature treemap accessors *)
val color_of_treemap_node :
  ('a, 'b) treemap -> Simple_color.color
val size_of_treemap_node :
  ('a, 'b) treemap  -> int
(*e: signature treemap accessors *)

(*s: signature algorithm accessors *)
val s_of_algo: algorithm -> string
val algo_of_s: string -> algorithm
(*e: signature algorithm accessors *)

(* tests *)
(*s: signature tree and treemap examples *)
val treemap_rectangles_ex:
   ((float * float) list * (float * float) list * (float * float * float)) list

val tree_ex_shneiderman_1991 : (unit, int) Common.tree
val tree_ex_wijk_1999: (unit, int) Common.tree
val treemap_ex_ordered_2001: (unit, unit) treemap
(*e: signature tree and treemap examples *)

val actions : unit -> Common.cmdline_actions

(*e: treemap.mli *)
