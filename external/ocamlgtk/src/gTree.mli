(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gTree.mli 1523 2010-07-25 12:42:26Z garrigue $ *)

open Gobject
open Gtk
open GObj
open GContainer

(** Tree and list widgets
   @gtkdoc gtk TreeWidget *)

(** {3 New GtkTreeView/Model framework} *)

type 'a column = {index: int; conv: 'a data_conv; creator: int}

class column_list :
  object
    method add : 'a data_conv -> 'a column
    method id : int
    method types : g_type list
    method lock : unit -> unit
  end

class row_reference : Gtk.row_reference -> model:[> `treemodel ] obj ->
  object
    method as_ref : Gtk.row_reference
    method iter : tree_iter
    method path : tree_path
    method valid : bool
  end

(** {4 Models} *)

(** @gtkdoc gtk GtkTreeModel *)
class model_signals : [> `treemodel] obj ->
  object ('a)
    method after : 'a
    method row_changed :
      callback:(tree_path -> tree_iter -> unit) -> GtkSignal.id
    method row_deleted : callback:(tree_path -> unit) -> GtkSignal.id
    method row_has_child_toggled :
      callback:(tree_path -> tree_iter -> unit) -> GtkSignal.id
    method row_inserted :
      callback:(tree_path -> tree_iter -> unit) -> GtkSignal.id
    method rows_reordered :
      callback:(tree_path -> tree_iter -> unit) -> GtkSignal.id
  end

val model_ids : (int,int) Hashtbl.t

(** @gtkdoc gtk GtkTreeModel *)
class model : ([> `treemodel] as 'a) obj ->
  object
    val obj : 'a obj
    val id : int
    method as_model : Gtk.tree_model
    method misc : gobject_ops
    method coerce : model
    method flags : GtkEnums.tree_model_flags list
    method n_columns : int
    method get_column_type : int -> Gobject.g_type
    method get_iter : tree_path -> tree_iter
    method get_path : tree_iter -> tree_path
    method get_row_reference : tree_path -> row_reference
    method get : row:tree_iter -> column:'b column -> 'b
    method get_iter_first : tree_iter option
    method iter_next : tree_iter -> bool
    method iter_has_child : tree_iter -> bool
    method iter_n_children : tree_iter option -> int
    method iter_children : ?nth:int -> tree_iter option -> tree_iter 
      (** @raise Invalid_argument if arguments do not designate a valid node *)
    method iter_parent : tree_iter -> tree_iter option
    method foreach : (tree_path -> tree_iter -> bool) -> unit
    method row_changed : tree_path -> tree_iter -> unit
  end

(** @gtkdoc gtk GtkTreeSortable *)
class tree_sortable_signals : ([> `treesortable|`treemodel] as 'a) obj ->
  object
    inherit model_signals
    method sort_column_changed : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkTreeSortable *)
class tree_sortable : ([> `treesortable|`treemodel] as 'a) obj ->
  object
    inherit model
    val obj : 'a obj
    method connect : tree_sortable_signals
    method sort_column_changed : unit -> unit
    method get_sort_column_id : (int * Gtk.Tags.sort_type) option
    method set_sort_column_id : int -> Gtk.Tags.sort_type -> unit
    method set_sort_func  : int -> (model -> Gtk.tree_iter -> Gtk.tree_iter -> int) -> unit
    method set_default_sort_func : (model -> Gtk.tree_iter -> Gtk.tree_iter -> int) -> unit
    method has_default_sort_func : bool
  end

(** Special value for the [#set_sort_column_id] method of {!GTree.tree_sortable}. *)

val default_sort_column_id  : int
val unsorted_sort_column_id : int

(** @gtkdoc gtk GtkTreeStore *)
class tree_store : Gtk.tree_store ->
  object
    inherit tree_sortable
    val obj : Gtk.tree_store
    method append : ?parent:tree_iter -> unit -> tree_iter
    method clear : unit -> unit
    method insert : ?parent:tree_iter -> int -> tree_iter
    method insert_after : ?parent:tree_iter -> tree_iter -> tree_iter
    method insert_before : ?parent:tree_iter -> tree_iter -> tree_iter
    method is_ancestor : iter:tree_iter -> descendant:tree_iter -> bool
    method iter_depth : tree_iter -> int
    method iter_is_valid : tree_iter -> bool (** @since GTK 2.2 *)
    method move_after : iter:tree_iter -> pos:tree_iter -> bool (** @since GTK 2.2 *)
    method move_before : iter:tree_iter -> pos:tree_iter -> bool (** @since GTK 2.2 *)
    method prepend : ?parent:tree_iter -> unit -> tree_iter
    method remove : tree_iter -> bool
    method set : row:tree_iter -> column:'a column -> 'a -> unit
    method swap : tree_iter -> tree_iter -> bool (** @since GTK 2.2 *)
  end

(** @gtkdoc gtk GtkTreeStore *)
val tree_store : column_list -> tree_store

(** @gtkdoc gtk GtkListStore *)
class list_store : Gtk.list_store ->
  object
    inherit tree_sortable
    val obj : Gtk.list_store
    method append : unit -> tree_iter
    method clear : unit -> unit
    method insert : int -> tree_iter
    method insert_after : tree_iter -> tree_iter
    method insert_before : tree_iter -> tree_iter
    method iter_is_valid : tree_iter -> bool (** @since GTK 2.2 *)
    method move_after : iter:tree_iter -> pos:tree_iter -> bool (** @since GTK 2.2 *)
    method move_before : iter:tree_iter -> pos:tree_iter -> bool (** @since GTK 2.2 *)
    method prepend : unit -> tree_iter
    method remove : tree_iter -> bool
    method set : row:tree_iter -> column:'a column -> 'a -> unit
    method swap : tree_iter -> tree_iter -> bool (** @since GTK 2.2 *)
  end

(** @gtkdoc gtk GtkListStore *)
val list_store : column_list -> list_store

(** Convenience function to map a caml list into a {!GTree.list_store} with a single column *)
val store_of_list : 'a Gobject.data_conv -> 'a list -> list_store * 'a column

(** @gtkdoc gtk GtkTreeModelSort *)
class model_sort : Gtk.tree_model_sort ->
  object
    inherit tree_sortable
    val obj : Gtk.tree_model_sort
    method model : model
    method convert_child_path_to_path : Gtk.tree_path -> Gtk.tree_path
    method convert_child_iter_to_iter : Gtk.tree_iter -> Gtk.tree_iter
    method convert_path_to_child_path : Gtk.tree_path -> Gtk.tree_path
    method convert_iter_to_child_iter : Gtk.tree_iter -> Gtk.tree_iter
    method reset_default_sort_func : unit -> unit
    method iter_is_valid : Gtk.tree_iter -> bool (** @since GTK 2.2 *)
  end

(** @gtkdoc gtk GtkTreeModelSort *)
val model_sort : #model -> model_sort
    
(** @since GTK 2.4
    @gtkdoc gtk GtkTreeModelFilter *)
class model_filter : Gtk.tree_model_filter ->
  object
    inherit model
    val obj : Gtk.tree_model_filter
    method connect : model_signals
    method child_model  : model
    method virtual_root : Gtk.tree_path
    method set_visible_func : (model -> Gtk.tree_iter -> bool) -> unit
    method set_visible_column : bool column -> unit
    method convert_child_path_to_path : Gtk.tree_path -> Gtk.tree_path
    method convert_child_iter_to_iter : Gtk.tree_iter -> Gtk.tree_iter
    method convert_path_to_child_path : Gtk.tree_path -> Gtk.tree_path
    method convert_iter_to_child_iter : Gtk.tree_iter -> Gtk.tree_iter
    method refilter : unit -> unit
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkTreeModelFilter *)
val model_filter : ?virtual_root:Gtk.tree_path -> #model -> model_filter


module Path : sig
  val create : int list -> Gtk.tree_path
  val copy : Gtk.tree_path -> Gtk.tree_path
  val get_indices : Gtk.tree_path -> int array
  val from_string : string -> Gtk.tree_path
  val to_string : Gtk.tree_path -> string
  val get_depth : Gtk.tree_path -> int
  val is_ancestor : Gtk.tree_path -> Gtk.tree_path -> bool

  (** {5 Mutating functions} *)

  val append_index : Gtk.tree_path -> int -> unit
  val prepend_index : Gtk.tree_path -> int -> unit
  val next : Gtk.tree_path -> unit
  val prev : Gtk.tree_path -> bool
  val up : Gtk.tree_path -> bool
  val down : Gtk.tree_path -> unit
end

(** {4 Selection} *)

(** @gtkdoc gtk GtkTreeSelection *)
class selection_signals : tree_selection ->
  object ('a)
    method after : 'a
    method changed : callback:(unit -> unit) -> GtkSignal.id
  end

(** The selection object for {!GTree.view}
   @gtkdoc gtk GtkTreeSelection *)
class selection :
  Gtk.tree_selection ->
  object
    val obj : Gtk.tree_selection
    method connect : selection_signals
    method misc : gobject_ops
    method count_selected_rows : int (** @since GTK 2.2 *)
    method get_mode : Tags.selection_mode
    method get_selected_rows : tree_path list
    method iter_is_selected : tree_iter -> bool
    method path_is_selected : tree_path -> bool
    method select_all : unit -> unit
    method select_iter : tree_iter -> unit
    method select_path : tree_path -> unit
    method select_range : tree_path -> tree_path -> unit
    method set_mode : Tags.selection_mode -> unit
    method set_select_function : (tree_path -> bool -> bool) -> unit
    method unselect_all : unit -> unit
    method unselect_iter : tree_iter -> unit
    method unselect_path : tree_path -> unit
    method unselect_range : tree_path -> tree_path -> unit (** @since GTK 2.2 *)
  end

(** {4 Views} *)

class type cell_renderer = object
  method as_renderer : Gtk.cell_renderer obj
end

(** @since GTK 2.4
    @gtkdoc gtk GtkCellLayout *)
class cell_layout : ([> Gtk.cell_layout] as 'a) Gtk.obj ->
  object
    method pack :
      ?expand:bool -> 
      ?from:Tags.pack_type -> #cell_renderer -> unit
   (** @param expand default value is [false]
       @param from default value is [`START] *)
    method reorder : #cell_renderer -> int -> unit
    method clear : unit -> unit
    method add_attribute : #cell_renderer -> string -> 'b column -> unit
    method clear_attributes : #cell_renderer -> unit
    method set_cell_data_func   : #cell_renderer -> (model -> Gtk.tree_iter -> unit) -> unit
    method unset_cell_data_func : #cell_renderer -> unit
  end

(** @gtkdoc gtk GtkTreeViewColumn *)
class view_column_signals : [> `gtk | `treeviewcolumn] obj ->
  object
    inherit GObj.gtkobj_signals
    method clicked : callback:(unit -> unit) -> GtkSignal.id
  end

(** A visible column in a {!GTree.view} widget
   @gtkdoc gtk GtkTreeViewColumn *)
class view_column : tree_view_column obj ->
  object
    inherit GObj.gtkobj
    inherit cell_layout
    val obj : tree_view_column obj
    method as_column : Gtk.tree_view_column obj
    method misc : GObj.gobject_ops
    method alignment : float
    method clickable : bool
    method connect : view_column_signals
    method fixed_width : int
    method get_sort_column_id : int
    method max_width : int
    method min_width : int
    method reorderable : bool
    method resizable : bool
    method set_alignment : float -> unit
    method set_clickable : bool -> unit
    method set_fixed_width : int -> unit
    method set_max_width : int -> unit
    method set_min_width : int -> unit
    method set_reorderable : bool -> unit
    method set_resizable : bool -> unit
    method set_sizing : Tags.tree_view_column_sizing -> unit
    method set_sort_column_id : int -> unit
    method set_sort_indicator : bool -> unit
    method set_sort_order : Tags.sort_type -> unit
    method set_title : string -> unit
    method set_visible : bool -> unit
    method set_widget : widget option -> unit
    method sizing : Tags.tree_view_column_sizing
    method sort_indicator : bool
    method sort_order : Tags.sort_type
    method title : string
    method visible : bool
    method widget : widget option
    method width : int
  end

(** @gtkdoc gtk GtkTreeViewColumn *)
val view_column :
  ?title:string ->
  ?renderer:(#cell_renderer * (string * 'a column) list) ->
  unit -> view_column

(** @gtkdoc gtk GtkTreeView *)
class view_signals : [> tree_view] obj ->
  object ('a)
    inherit GContainer.container_signals
    method columns_changed : callback:(unit -> unit) -> GtkSignal.id
    method cursor_changed : callback:(unit -> unit) -> GtkSignal.id
    method expand_collapse_cursor_row :
      callback:(logical:bool -> expand:bool -> all:bool -> bool) ->
      GtkSignal.id
    method move_cursor :
      callback:(Tags.movement_step -> int -> bool) -> GtkSignal.id
    method row_activated :
      callback:(tree_path -> view_column -> unit) -> GtkSignal.id
    method row_collapsed :
      callback:(tree_iter -> tree_path -> unit) -> GtkSignal.id
    method row_expanded :
      callback:(tree_iter -> tree_path -> unit) -> GtkSignal.id
    method select_all : callback:(unit -> bool) -> GtkSignal.id
    method select_cursor_parent : callback:(unit -> bool) -> GtkSignal.id
    method select_cursor_row :
      callback:(start_editing:bool -> bool) -> GtkSignal.id
    method set_scroll_adjustments :
      callback:(GData.adjustment option -> GData.adjustment option -> unit) ->
      GtkSignal.id
    method start_interactive_search : callback:(unit -> bool) -> GtkSignal.id
    method test_collapse_row :
      callback:(tree_iter -> tree_path -> bool) -> GtkSignal.id
    method test_expand_row :
      callback:(tree_iter -> tree_path -> bool) -> GtkSignal.id
    method toggle_cursor_row : callback:(unit -> bool) -> GtkSignal.id
    method unselect_all : callback:(unit -> bool) -> GtkSignal.id
  end

(** A widget for displaying both trees and lists
   @gtkdoc gtk GtkTreeView *)
class view : tree_view obj ->
  object
    inherit GContainer.container
    val obj : tree_view obj
    method as_tree_view : Gtk.tree_view Gtk.obj
    method connect : view_signals
    method append_column : view_column -> int
    method collapse_all : unit -> unit
    method collapse_row : tree_path -> unit
    method enable_search : bool
    method event : GObj.event_ops
    method expand_all : unit -> unit
    method expand_row : ?all:bool -> tree_path -> unit
    (** @param all default value is [false] *)
    method expand_to_path : tree_path -> unit (** @since GTK 2.2 *)
    method expander_column : view_column option
    method fixed_height_mode : bool
    method get_column : int -> view_column
    method get_cursor : unit -> tree_path option * view_column option
    method get_path_at_pos :
      x:int -> y:int -> (tree_path * view_column * int * int) option
    method get_cell_area :
        ?path:tree_path -> ?col:view_column -> unit -> Gdk.Rectangle.t
    method get_visible_range : unit -> (tree_path * tree_path) option
    method hadjustment : GData.adjustment
    method headers_visible : bool
    method insert_column : view_column -> int -> int
    method model : model
    method move_column : view_column -> after:view_column -> int
    method remove_column : view_column -> int
    method reorderable : bool
    method row_activated : tree_path -> view_column -> unit
    method row_expanded : tree_path -> bool
    method rules_hint : bool
    method scroll_to_cell :
      ?align:float * float -> tree_path -> view_column -> unit
    method scroll_to_point : int -> int -> unit
    method search_column : int
    method selection : selection
    method set_cursor :
      ?cell:#cell_renderer ->
      ?edit:bool -> tree_path -> view_column -> unit (** @since GTK 2.2 *)
    (** @param edit default value is [false] *)
    method set_enable_search : bool -> unit
    method set_expander_column : view_column option -> unit
    method set_fixed_height_mode : bool -> unit
    method set_hadjustment : GData.adjustment -> unit
    method set_headers_clickable : bool -> unit
    method set_headers_visible : bool -> unit
    method set_model : model option -> unit
    method set_reorderable : bool -> unit
    method set_rules_hint : bool -> unit
    method set_search_column : int -> unit
    method set_tooltip_column : int -> unit
    method set_vadjustment : GData.adjustment -> unit
    method tooltip_column : int
    method vadjustment : GData.adjustment

    method hover_expand : bool (** @since GTK 2.6 *)
    method set_hover_expand : bool -> unit (** @since GTK 2.6 *)
    method hover_selection : bool (** @since GTK 2.6 *)
    method set_hover_selection : bool -> unit (** @since GTK 2.6 *)
    method set_row_separator_func : (model -> tree_iter -> bool) option -> unit (** @since GTK 2.6 *)
  end

(** @gtkdoc gtk GtkTreeView *)
val view :
  ?model:#model ->
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?enable_search:bool ->
  ?fixed_height_mode:bool ->
  ?headers_clickable:bool ->
  ?headers_visible:bool ->
  ?reorderable:bool ->
  ?rules_hint:bool ->
  ?search_column:int ->
  ?tooltip_column:int ->
  ?border_width:int -> ?width:int -> ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> view
(** @param enable_search default value is [true]
    @param fixed_height_mode default value is [false]
    @param headers_clickable default value is [false]
    @param headers_visible default value is [true]
    @param reorderable default value is [false]
    @param rules_hint default value is [false] *)

(** {4 Cell Renderers} *)

type cell_properties =
  [ `CELL_BACKGROUND of string
  | `CELL_BACKGROUND_GDK of Gdk.color
  | `CELL_BACKGROUND_SET of bool
  | `HEIGHT of int
  | `IS_EXPANDED of bool
  | `IS_EXPANDER of bool
  | `MODE of Tags.cell_renderer_mode
  | `VISIBLE of bool
  | `WIDTH of int
  | `XALIGN of float
  | `XPAD of int
  | `YALIGN of float
  | `YPAD of int ]
type cell_properties_pixbuf =
  [ cell_properties
  | `PIXBUF of GdkPixbuf.pixbuf
  | `PIXBUF_EXPANDER_CLOSED of GdkPixbuf.pixbuf
  | `PIXBUF_EXPANDER_OPEN of GdkPixbuf.pixbuf
  | `STOCK_DETAIL of string
  | `STOCK_ID of string
  | `STOCK_SIZE of Tags.icon_size ] 
type cell_properties_text =
  [ cell_properties
  | `BACKGROUND of string
  | `BACKGROUND_GDK of Gdk.color
  | `BACKGROUND_SET of bool
  | `EDITABLE of bool
  | `FAMILY of string
  | `FONT of string
  | `FONT_DESC of Pango.font_description
  | `FOREGROUND of string
  | `FOREGROUND_GDK of Gdk.color
  | `FOREGROUND_SET of bool
  | `MARKUP of string
  | `RISE of int
  | `SCALE of Pango.Tags.scale
  | `SINGLE_PARAGRAPH_MODE of bool
  | `SIZE of int
  | `SIZE_POINTS of float
  | `STRETCH of Pango.Tags.stretch
  | `STRIKETHROUGH of bool
  | `STYLE of Pango.Tags.style
  | `TEXT of string
  | `UNDERLINE of Pango.Tags.underline
  | `VARIANT of Pango.Tags.variant
  | `WEIGHT of Pango.Tags.weight ]
type cell_properties_toggle =
  [ cell_properties
  | `ACTIVATABLE of bool
  | `ACTIVE of bool
  | `INCONSISTENT of bool
  | `RADIO of bool ]
type cell_properties_progress =
  [ cell_properties 
  | `VALUE of int
  | `TEXT of string option ]
type cell_properties_combo =
  [ cell_properties_text
  | `MODEL of model option
  | `TEXT_COLUMN of string column
  | `HAS_ENTRY of bool ]
type cell_properties_accel = 
 [ cell_properties_text 
  | `KEY of Gdk.keysym
  | `ACCEL_MODE of GtkEnums.cell_renderer_accel_mode
  | `MODS of GdkEnums.modifier list
  | `KEYCODE of int ]

(** @gtkdoc gtk GtkCellRenderer *)
class type ['a, 'b] cell_renderer_skel =
  object
    inherit GObj.gtkobj
    val obj : 'a obj
    method as_renderer : Gtk.cell_renderer obj
    method get_property : ('a, 'c) property -> 'c
    method set_properties : 'b list -> unit
  end

(** @gtkdoc gtk GtkCellRenderer *)
class virtual ['a, 'b] cell_renderer_impl : ([>Gtk.cell_renderer] as 'a) obj ->
  object
    inherit ['a,'b] cell_renderer_skel
    method private virtual param : 'b -> 'a param
  end

(** @gtkdoc gtk GtkCellRendererPixbuf *)
class cell_renderer_pixbuf : Gtk.cell_renderer_pixbuf obj ->
  object
    inherit[Gtk.cell_renderer_pixbuf,cell_properties_pixbuf] cell_renderer_skel
    method connect : GObj.gtkobj_signals_impl
  end

(** @gtkdoc gtk GtkCellRendererText *)
class cell_renderer_text_signals : ([>Gtk.cell_renderer_text] as 'a) obj ->
  object
    inherit GObj.gtkobj_signals
    val obj : 'a obj
    method edited : callback:(Gtk.tree_path -> string -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkCellRendererText *)
class cell_renderer_text : Gtk.cell_renderer_text obj ->
  object
    inherit [Gtk.cell_renderer_text,cell_properties_text] cell_renderer_skel
    method connect : cell_renderer_text_signals
    method set_fixed_height_from_font : int -> unit
  end

(** @gtkdoc gtk GtkCellRendererToggle *)
class cell_renderer_toggle_signals :  Gtk.cell_renderer_toggle obj ->
  object
    inherit GObj.gtkobj_signals
    method toggled : callback:(Gtk.tree_path -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkCellRendererToggle *)
class cell_renderer_toggle : Gtk.cell_renderer_toggle obj ->
  object
    inherit[Gtk.cell_renderer_toggle,cell_properties_toggle] cell_renderer_skel
    method connect : cell_renderer_toggle_signals
  end

(** @since GTK 2.6
    @gtkdoc gtk GtkCellRendererProgress *)
class cell_renderer_progress : Gtk.cell_renderer_progress obj ->
  object
    inherit[Gtk.cell_renderer_progress,cell_properties_progress] cell_renderer_skel
    method connect : GObj.gtkobj_signals_impl
  end

(** @since GTK 2.6
    @gtkdoc gtk GtkCellRendererCombo *)
class cell_renderer_combo_signals : ([>Gtk.cell_renderer_combo] as 'a) obj ->
  object
    inherit cell_renderer_text_signals
    val obj : 'a obj
    method changed :
      callback:(Gtk.tree_path -> Gtk.tree_iter -> unit) -> GtkSignal.id
  end

(** @since GTK 2.6
    @gtkdoc gtk GtkCellRendererCombo *)
class cell_renderer_combo : Gtk.cell_renderer_combo obj ->
  object
    inherit[Gtk.cell_renderer_combo,cell_properties_combo] cell_renderer_skel
    method connect : cell_renderer_combo_signals
    method set_fixed_height_from_font : int -> unit
  end

(** @since GTK 2.10
    @gtkdoc gtk GtkCellRendererText *)
class cell_renderer_accel_signals : Gtk.cell_renderer_accel obj ->
  object
    inherit GObj.gtkobj_signals
    method edited : callback:(Gtk.tree_path -> string -> unit) -> GtkSignal.id
    method accel_edited :
      callback:(tree_path -> accel_key:int -> accel_mods:int 
		 -> hardware_keycode:int -> unit) 
      -> GtkSignal.id
    method accel_cleared : callback:(tree_path -> unit) -> GtkSignal.id

  end

(** @since GTK 2.10
    @gtkdoc gtk GtkCellRendererAccel *)
class cell_renderer_accel : Gtk.cell_renderer_accel obj ->
  object
    inherit[Gtk.cell_renderer_accel,cell_properties_accel] cell_renderer_skel
    method connect : cell_renderer_accel_signals

  end

(** @gtkdoc gtk GtkCellRendererPixbuf *)
val cell_renderer_pixbuf : cell_properties_pixbuf list -> cell_renderer_pixbuf

(** @gtkdoc gtk GtkCellRendererText *)
val cell_renderer_text : cell_properties_text list -> cell_renderer_text

(** @gtkdoc gtk GtkCellRendererToggle *)
val cell_renderer_toggle : cell_properties_toggle list -> cell_renderer_toggle

(** @since GTK 2.6 
    @gtkdoc gtk GtkCellRendererProgress *)
val cell_renderer_progress : cell_properties_progress list -> cell_renderer_progress

(** @since GTK 2.6 
    @gtkdoc gtk GtkCellRendererCombo *)
val cell_renderer_combo : cell_properties_combo list -> cell_renderer_combo

(** @since GTK 2.10
    @gtkdoc gtk GtkCellRendererAccel *)
val cell_renderer_accel : cell_properties_accel list -> cell_renderer_accel

(** {3 GtkIconView} *)

(** @gtkdoc gtk GtkIconView
    @since GTK 2.6 *)
class icon_view_signals : [> Gtk.icon_view] Gtk.obj ->
  object
    inherit GContainer.container_signals
    method item_activated : callback:(Gtk.tree_path -> unit) -> GtkSignal.id
    method selection_changed : callback:(unit -> unit) -> GtkSignal.id
  end

(** A widget which displays a list of icons in a grid
    @gtkdoc gtk GtkIconView
    @since GTK 2.6 *)
class icon_view :
  ([> Gtk.icon_view] as 'a) Gtk.obj ->
  object
    inherit GContainer.container
    val obj : 'a Gtk.obj
    method connect : icon_view_signals
    method event : GObj.event_ops

    (** Properties *)

    method model : model
    method set_model : model option -> unit
    method set_markup_column : string column -> unit
    method set_pixbuf_column : GdkPixbuf.pixbuf column -> unit
    method set_text_column : string column -> unit
    method orientation : GtkEnums.orientation
    method set_orientation : GtkEnums.orientation -> unit
    method selection_mode : GtkEnums.selection_mode
    method set_selection_mode : GtkEnums.selection_mode -> unit
    method column_spacing : int
    method set_column_spacing : int -> unit
    method item_width : int
    method set_item_width : int -> unit
    method margin : int
    method set_margin : int -> unit
    method columns : int
    method set_columns : int -> unit
    method row_spacing : int
    method set_row_spacing : int -> unit
    method spacing : int
    method set_spacing : int -> unit

    method get_path_at_pos : int -> int -> Gtk.tree_path
    method selected_foreach : (Gtk.tree_path -> unit) -> unit
    method get_selected_items : Gtk.tree_path list
    method path_is_selected : Gtk.tree_path -> bool
    method select_path : Gtk.tree_path -> unit
    method unselect_path : Gtk.tree_path -> unit
    method select_all : unit -> unit
    method unselect_all : unit -> unit

    method item_activated : Gtk.tree_path -> unit
  end

(** A widget which displays a list of icons in a grid
    @gtkdoc gtk GtkIconView
    @since GTK 2.6 *)
val icon_view :
  ?model:#model ->
  ?columns:int ->
  ?orientation:GtkEnums.orientation ->
  ?selection_mode:GtkEnums.selection_mode ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> icon_view


class type virtual ['obj,'row,'a,'b,'c] custom_tree_model_type = 
object
  inherit model
  val obj : 'obj
  method connect : model_signals

  (** Signal emitters *)
  method custom_row_changed : Gtk.tree_path -> 'row -> unit
  method custom_row_deleted : Gtk.tree_path -> unit
  method custom_row_has_child_toggled :
    Gtk.tree_path -> 'row -> unit
  method custom_row_inserted : Gtk.tree_path -> 'row -> unit
  method custom_rows_reordered :
    Gtk.tree_path -> 'row option -> int array -> unit

  (** Override these to implement a cache of rows *)
  method custom_unref_node : 'row -> unit
  method custom_ref_node : 'row -> unit

  method custom_flags : GtkEnums.tree_model_flags list

  (** Functions of the custom model. They must act exactly as described in the documentation 
      of Gtk orelse Gtk may emit fatal errors. *)
  method virtual custom_get_iter : Gtk.tree_path -> 'row option
  method virtual custom_get_path : 'row -> Gtk.tree_path
  method virtual custom_value : Gobject.g_type -> 'row -> column:int -> Gobject.basic

    (** [custom_value typ row ~column] is the value to set in [row] for column [column].
        It must must be of the type [typ], i.e. the type declared for column  [column]. *)
    
  method virtual custom_iter_children : 'row option -> 'row option
  method virtual custom_iter_has_child : 'row -> bool
  method virtual custom_iter_n_children : 'row option -> int
  method virtual custom_iter_next : 'row -> 'row option
  method virtual custom_iter_nth_child : 'row option -> int -> 'row option
  method virtual custom_iter_parent : 'row -> 'row option

  method virtual custom_decode_iter : 'a -> 'b -> 'c -> 'row
  method virtual custom_encode_iter : 'row -> 'a * 'b * 'c

  (** For internal use only. Do not override these methods. *)
  method custom_n_columns : int
  method custom_get_column_type : int -> Gobject.g_type
  method custom_get_value :
    'row -> int -> Gobject.g_value -> unit

end

(** A base class to inherit from to make a custom tree model. *)
class virtual ['row,'a,'b,'c] custom_tree_model : 
  column_list -> [Gtk.tree_model_custom,'row,'a,'b,'c] custom_tree_model_type
  
