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

(* $Id: gTree.ml 1523 2010-07-25 12:42:26Z garrigue $ *)

open StdLabels
open Gaux
open Gobject
open Gtk
open GtkBase
open GtkTree
open OgtkBaseProps
open OgtkTreeProps
open GObj
open GContainer

(* New GtkTreeView/Model framework *)

type 'a column = {index: int; conv: 'a data_conv; creator: int}

class column_list = object (self)
  val mutable index = 0
  val mutable types = []
  val mutable locked = false
  method types = List.rev types
  method add : 'a. 'a data_conv -> 'a column = fun conv ->
    if locked then failwith "GTree.column_list#add";
    let n = index in
    types <- Data.get_type conv :: types;
    index <- index + 1;
    {index = n; conv = conv; creator = Oo.id self}
  method id = Oo.id self
  method lock () = locked <- true
end

class row_reference rr ~model = object (self)
  method as_ref = rr
  method path = RowReference.get_path rr
  method valid = RowReference.valid rr
  method iter = TreeModel.get_iter model self#path
end

class model_signals obj = object
  inherit ['a] gobject_signals obj
  inherit tree_model_sigs
end

let model_ids = Hashtbl.create 7
let custom_model_ids = Hashtbl.create 7

class model obj = object (self)
  val id =
    try Hashtbl.find model_ids (Gobject.get_oid obj) 
    with Not_found -> 0
  val obj = obj
  method as_model = (obj :> tree_model)
  method coerce = (self :> model)
  method misc = new gobject_ops obj
  method flags = TreeModel.get_flags obj
  method n_columns = TreeModel.get_n_columns obj
  method get_column_type = TreeModel.get_column_type obj
  method get_iter = TreeModel.get_iter obj
  method get_path = TreeModel.get_path obj
  method get_row_reference path =
    new row_reference (RowReference.create obj path) obj
  method get : 'a. row:tree_iter -> column:'a column -> 'a =
    fun ~row ~column ->
      if column.creator <> id then invalid_arg "GTree.model#get: bad column";
      (* Prevent a class derived from an ancestor of a custom model from calling 
         get: this would be unsound. *)
      if not (Gobject.is_a obj "Custom_model") 
        && Hashtbl.mem custom_model_ids id 
      then invalid_arg "GTree.model#get: embedded custom model for iterator. Please use model#get_path then custom_model#custom_get_iter.";
      let v = Value.create_empty () in
      TreeModel.get_value obj ~row ~column:column.index v;
      Data.of_value column.conv v
  method get_iter_first = TreeModel.get_iter_first obj
  method iter_next = TreeModel.iter_next obj
  method iter_has_child = TreeModel.iter_has_child obj
  method iter_n_children = TreeModel.iter_n_children obj
  method iter_children = TreeModel.iter_children obj
  method iter_parent = TreeModel.iter_parent obj
  method foreach = TreeModel.foreach obj
  method row_changed = TreeModel.row_changed obj
end

class tree_sortable_signals obj = object
  inherit model_signals obj
  inherit tree_sortable_sigs
end

class tree_sortable obj = object
  inherit model obj
  method connect = new tree_sortable_signals obj
  method sort_column_changed () = GtkTree.TreeSortable.sort_column_changed obj
  method get_sort_column_id = GtkTree.TreeSortable.get_sort_column_id obj
  method set_sort_column_id = GtkTree.TreeSortable.set_sort_column_id obj
  method set_sort_func id cmp = 
    GtkTree.TreeSortable.set_sort_func obj id
      (fun m it_a it_b -> cmp (new model m) it_a it_b)
  method set_default_sort_func cmp = 
    GtkTree.TreeSortable.set_default_sort_func obj
      (fun m it_a it_b -> cmp (new model m) it_a it_b)
  method has_default_sort_func = GtkTree.TreeSortable.has_default_sort_func obj
end

let default_sort_column_id  = -1
let unsorted_sort_column_id = -2

class tree_store obj = object
  inherit tree_sortable obj
  method set : 'a. row:tree_iter -> column:'a column -> 'a -> unit =
    fun ~row ~column data ->
      if column.creator <> id then
        invalid_arg "GTree.tree_store#set: bad column";
      TreeStore.set_value obj ~row ~column:column.index
        (Data.to_value column.conv data)
  method remove = TreeStore.remove obj
  method insert = TreeStore.insert obj
  method insert_before = TreeStore.insert_before obj
  method insert_after = TreeStore.insert_after obj
  method append = TreeStore.append obj
  method prepend = TreeStore.prepend obj
  method is_ancestor = TreeStore.is_ancestor obj
  method iter_depth = TreeStore.iter_depth obj
  method clear () = TreeStore.clear obj
  method iter_is_valid = TreeStore.iter_is_valid obj
  method swap = TreeStore.swap obj
  method move_before = TreeStore.move_before obj
  method move_after = TreeStore.move_after obj
end

let tree_store (cols : column_list) =
  cols#lock ();
  let store = TreeStore.create (Array.of_list cols#types) in
  Hashtbl.add model_ids(Gobject.get_oid store) cols#id;
  new tree_store store

class list_store obj = object
  inherit tree_sortable obj
  method set : 'a. row:tree_iter -> column:'a column -> 'a -> unit =
    fun ~row ~column data ->
      if column.creator <> id then
        invalid_arg "GTree.list_store#set: bad column";
      ListStore.set_value obj ~row ~column:column.index
        (Data.to_value column.conv data)
  method remove = ListStore.remove obj
  method insert = ListStore.insert obj
  method insert_before = ListStore.insert_before obj
  method insert_after = ListStore.insert_after obj
  method append = ListStore.append obj
  method prepend = ListStore.prepend obj
  method clear () = ListStore.clear obj
  method iter_is_valid = ListStore.iter_is_valid obj
  method swap = ListStore.swap obj
  method move_before = ListStore.move_before obj
  method move_after = ListStore.move_after obj
end

let list_store (cols : column_list) =
  cols#lock ();
  let store = ListStore.create (Array.of_list cols#types) in
  Hashtbl.add model_ids (Gobject.get_oid store) cols#id;
  new list_store store

let store_of_list conv data =
  let cols = new column_list in
  let column = cols#add conv in
  let store = list_store cols in
  List.iter
    (fun d ->
      let row = store#append () in
      store#set ~row ~column d)
    data ;
  store, column

class model_sort (obj : Gtk.tree_model_sort) = object
  inherit tree_sortable obj
  method model = new model (Gobject.get GtkTree.TreeModelSort.P.model obj)
  method convert_child_path_to_path = GtkTree.TreeModelSort.convert_child_path_to_path obj
  method convert_child_iter_to_iter = GtkTree.TreeModelSort.convert_child_iter_to_iter obj
  method convert_path_to_child_path = GtkTree.TreeModelSort.convert_path_to_child_path obj
  method convert_iter_to_child_iter = GtkTree.TreeModelSort.convert_iter_to_child_iter obj
  method reset_default_sort_func () = GtkTree.TreeModelSort.reset_default_sort_func obj
  method iter_is_valid = GtkTree.TreeModelSort.iter_is_valid obj
end

let model_sort model =
  let child_model = model#as_model in
  let child_oid = Gobject.get_oid child_model in
  let o = GtkTree.TreeModelSort.create ~model:child_model [] in
  begin try 
    let child_id = Hashtbl.find model_ids child_oid in
    Hashtbl.add model_ids (Gobject.get_oid o) child_id
  with Not_found -> ()
  end ; 
  new model_sort o

class model_filter (obj : Gtk.tree_model_filter) = object
  inherit model obj
  method connect = new model_signals obj
  method child_model  = new model (Gobject.get GtkTree.TreeModelFilter.P.child_model obj)
  method virtual_root = Gobject.get GtkTree.TreeModelFilter.P.virtual_root obj
  method set_visible_func f =
    GtkTree.TreeModelFilter.set_visible_func obj
      (fun o it -> f (new model o) it)
  method set_visible_column (c : bool column) = 
    GtkTree.TreeModelFilter.set_visible_column obj c.index
  method convert_child_path_to_path = GtkTree.TreeModelFilter.convert_child_path_to_path obj
  method convert_child_iter_to_iter = GtkTree.TreeModelFilter.convert_child_iter_to_iter obj
  method convert_path_to_child_path = GtkTree.TreeModelFilter.convert_path_to_child_path obj
  method convert_iter_to_child_iter = GtkTree.TreeModelFilter.convert_iter_to_child_iter obj
  method refilter () = GtkTree.TreeModelFilter.refilter obj
end

let model_filter ?virtual_root model =
  let child_model = model#as_model in
  let child_oid = Gobject.get_oid child_model in
  let o = GtkTree.TreeModelFilter.create ~child_model ?virtual_root [] in
  begin try 
    let child_id = Hashtbl.find model_ids child_oid in
    Hashtbl.add model_ids (Gobject.get_oid o) child_id;
  with Not_found -> ()
  end ; 
  new model_filter o

module Path = TreePath

(*
open GTree.Data;;
let cols = new GTree.column_list ;;
let title = cols#add string;;
let author = cols#add string;;
let checked = cols#add boolean;;
let store = new GTree.tree_store cols;;
*)

class type cell_renderer = object
  method as_renderer : Gtk.cell_renderer obj
end

class cell_layout obj = object
  method pack :
    'a. ?expand:bool -> ?from:Tags.pack_type -> (#cell_renderer as 'a) -> unit =
      fun ?expand ?from crr -> GtkTree.CellLayout.pack obj ?expand ?from crr#as_renderer
  method reorder : 
    'a. (#cell_renderer as 'a) -> int -> unit = 
       fun crr pos -> GtkTree.CellLayout.reorder obj crr#as_renderer pos
  method clear () = GtkTree.CellLayout.clear obj
  method add_attribute :
    'a 'b. (#cell_renderer as 'a) -> string -> 'b column -> unit =
      fun crr attr col ->
        GtkTree.CellLayout.add_attribute obj crr#as_renderer attr col.index
  method set_cell_data_func :
    'a. (#cell_renderer as 'a) -> (model -> Gtk.tree_iter -> unit) -> unit =
    fun crr cb -> 
      GtkTree.CellLayout.set_cell_data_func obj crr#as_renderer
	(Some (fun m i -> cb (new model m) i))
  method unset_cell_data_func : 'a. (#cell_renderer as 'a) -> unit = 
    fun crr ->
      GtkTree.CellLayout.set_cell_data_func obj crr#as_renderer None
  method clear_attributes :
    'a. (#cell_renderer as 'a) -> unit = 
      fun crr -> GtkTree.CellLayout.clear_attributes obj crr#as_renderer
end

class view_column_signals obj = object (self)
  inherit gtkobj_signals_impl obj
  method clicked = self#connect TreeViewColumn.S.clicked
end

module P = TreeViewColumn.P
class view_column (_obj : tree_view_column obj) = object
  inherit GObj.gtkobj _obj
  method private obj = _obj
  inherit tree_view_column_props
  method as_column = obj
  method misc = new gobject_ops obj
  method connect = new view_column_signals obj

  (* in GTK 2.4 this will be in GtkCellLayout interface *)
  (* inherit cell_layout _obj *)
  method clear () = TreeViewColumn.clear obj
  method reorder :
    'a. (#cell_renderer as 'a) -> int -> unit = 
    fun crr pos -> GtkTree.CellLayout.reorder obj crr#as_renderer pos
  method pack : 'a. ?expand:_ -> ?from:_ -> (#cell_renderer as 'a)-> _ =
    fun ?expand ?from  r -> TreeViewColumn.pack obj ?expand ?from r#as_renderer
  method add_attribute :
    'a 'b. (#cell_renderer as 'a) -> string -> 'b column -> unit =
    fun crr attr col ->
      TreeViewColumn.add_attribute obj crr#as_renderer attr col.index
  method clear_attributes : 
    'a. (#cell_renderer as 'a) -> unit = 
    fun crr -> TreeViewColumn.clear_attributes obj crr#as_renderer

  method set_sort_column_id = TreeViewColumn.set_sort_column_id obj
  method get_sort_column_id = TreeViewColumn.get_sort_column_id obj
  method set_cell_data_func :
    'a. (#cell_renderer as 'a) -> (model -> Gtk.tree_iter -> unit) -> unit =
    fun crr cb -> 
      TreeViewColumn.set_cell_data_func obj crr#as_renderer
	(Some (fun m i -> cb (new model m) i))
  method unset_cell_data_func : 'a. (#cell_renderer as 'a) -> unit = 
    fun crr ->
      TreeViewColumn.set_cell_data_func obj crr#as_renderer None
end
let view_column ?title ?renderer () =
  let w = new view_column (TreeViewColumn.create []) in
  may title ~f:w#set_title;
  may renderer ~f:
    begin fun (crr, l) ->
      w#pack crr;
      List.iter l ~f:
        (fun (attr,col) -> w#add_attribute crr attr col)
    end;
  w

let as_column (col : view_column) = col#as_column

class selection_signals (obj : tree_selection) = object (self)
  inherit ['a] gobject_signals obj
  method changed = self#connect TreeSelection.S.changed
end

class selection obj = object
  val obj = obj
  method connect = new selection_signals obj
  method misc = new gobject_ops obj
  method set_mode = TreeSelection.set_mode obj
  method get_mode = TreeSelection.get_mode obj
  method set_select_function = TreeSelection.set_select_function obj
  method get_selected_rows = TreeSelection.get_selected_rows obj
  method count_selected_rows = TreeSelection.count_selected_rows obj
  method select_path = TreeSelection.select_path obj
  method unselect_path = TreeSelection.unselect_path obj
  method path_is_selected = TreeSelection.path_is_selected obj
  method select_iter = TreeSelection.select_iter obj
  method unselect_iter = TreeSelection.unselect_iter obj
  method iter_is_selected = TreeSelection.iter_is_selected obj
  method select_all () = TreeSelection.select_all obj
  method unselect_all () = TreeSelection.unselect_all obj
  method select_range = TreeSelection.select_range obj
  method unselect_range = TreeSelection.unselect_range obj
end

class view_signals obj = object (self)
  inherit container_signals_impl obj
  inherit tree_view_sigs
  method row_activated ~callback =
    self#connect TreeView.S.row_activated
      ~callback:(fun it vc -> callback it (new view_column vc))

end

open TreeView.P
class view obj = object
  inherit [Gtk.tree_view] GContainer.container_impl obj
  inherit tree_view_props
  method as_tree_view = (obj :> Gtk.tree_view Gtk.obj)
  method connect = new view_signals obj
  method event = new GObj.event_ops obj
  method selection = new selection (TreeView.get_selection obj)
  method expander_column = may_map (new view_column) (get expander_column obj)
  method set_expander_column c =
    set expander_column obj (may_map as_column c)
  method model = new model (Property.get_some obj model)
  method set_model m = set model obj (may_map (fun (m:model) -> m#as_model) m)
  method append_column col = TreeView.append_column obj (as_column col)
  method remove_column col = TreeView.remove_column obj (as_column col)
  method insert_column col = TreeView.insert_column obj (as_column col)
  method get_column n = new view_column (TreeView.get_column obj n)
  method move_column col ~after =
    TreeView.move_column_after obj (as_column col) (as_column after)
  method scroll_to_point = TreeView.scroll_to_point obj
  method scroll_to_cell ?align path col =
    TreeView.scroll_to_cell obj ?align path (as_column col)
  method row_activated path col =
    TreeView.row_activated obj path (as_column col)
  method expand_all () = TreeView.expand_all obj
  method collapse_all () = TreeView.collapse_all obj
  method expand_row ?(all=false) = TreeView.expand_row obj ~all
  method expand_to_path = TreeView.expand_to_path obj
  method collapse_row = TreeView.collapse_row obj
  method row_expanded = TreeView.row_expanded obj
  method set_cursor : 'a. ?cell:(#cell_renderer as 'a) -> _ =
    fun ?cell ?(edit=false) row col ->
      match cell with
        None -> TreeView.set_cursor obj ~edit row (as_column col)
      | Some cell ->
          TreeView.set_cursor_on_cell obj ~edit row (as_column col)
            cell#as_renderer
  method get_cursor () =
    match TreeView.get_cursor obj with
      path, Some vc -> path, Some (new view_column vc)
    | _, None as pair -> pair
  method get_path_at_pos ~x ~y =
    match TreeView.get_path_at_pos obj ~x ~y with
      Some (p, c, x, y) -> Some (p, new view_column c, x, y)
    | None -> None
  method get_cell_area ?path ?col () =
    TreeView.get_cell_area obj ?path ?col:(Gaux.may_map as_column col) ()
  method get_visible_range () =
    TreeView.get_visible_range obj
  method set_row_separator_func fo =
    TreeView.set_row_separator_func obj 
      (Gaux.may_map (fun f m -> f (new model m)) fo)
end
let view ?model ?hadjustment ?vadjustment =
  let model = may_map (fun m -> m#as_model) model in
  let hadjustment = may_map GData.as_adjustment hadjustment in
  let vadjustment = may_map GData.as_adjustment vadjustment in
  TreeView.make_params [] ?model ?hadjustment ?vadjustment ~cont:(
  GContainer.pack_container ~create:(fun p -> new view (TreeView.create p)))

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
type cell_properties_pixbuf_only =
  [ `PIXBUF of GdkPixbuf.pixbuf
  | `PIXBUF_EXPANDER_CLOSED of GdkPixbuf.pixbuf
  | `PIXBUF_EXPANDER_OPEN of GdkPixbuf.pixbuf
  | `STOCK_DETAIL of string
  | `STOCK_ID of string
  | `STOCK_SIZE of Gtk.Tags.icon_size ] 
type cell_properties_pixbuf = [ cell_properties | cell_properties_pixbuf_only ]
type cell_properties_text_only =
  [ `BACKGROUND of string
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
  | `SINGLE_PARAGRAPH_MODE of bool
  | `SIZE of int
  | `SIZE_POINTS of float
  | `STRETCH of Pango.Tags.stretch
  | `STRIKETHROUGH of bool
  | `STYLE of Pango.Tags.style
  | `TEXT of string
  | `UNDERLINE of Pango.Tags.underline
  | `VARIANT of Pango.Tags.variant ]
type cell_properties_text =
  [ cell_properties
  | cell_properties_text_only
  | `SCALE of Pango.Tags.scale
  | `WEIGHT of Pango.Tags.weight ]
type cell_properties_toggle_only =
  [ `ACTIVATABLE of bool
  | `ACTIVE of bool
  | `INCONSISTENT of bool
  | `RADIO of bool ]
type cell_properties_toggle = [ cell_properties | cell_properties_toggle_only ]
type cell_properties_progress_only =
  [ `VALUE of int
  | `TEXT of string option ]
type cell_properties_progress = [ cell_properties | cell_properties_progress_only ]
type cell_properties_combo_only =
  [ `MODEL of model option
  | `TEXT_COLUMN of string column
  | `HAS_ENTRY of bool ]
type cell_properties_combo = [ cell_properties_text | cell_properties_combo_only ]

type cell_properties_accel_only =
  [ `KEY of int
  | `ACCEL_MODE of GtkEnums.cell_renderer_accel_mode
  | `MODS of GdkEnums.modifier list
  | `KEYCODE of int ]

type cell_properties_accel = [ cell_properties_text | cell_properties_accel_only ]

let cell_renderer_pixbuf_param' = function
  | #cell_properties_pixbuf_only as x -> cell_renderer_pixbuf_param x
  | #cell_properties as x -> cell_renderer_param x
let cell_renderer_text_param' = function
  | `SCALE s -> cell_renderer_text_param (`SCALE (Pango.Tags.scale_to_float s))
  | `WEIGHT w -> cell_renderer_text_param(`WEIGHT (Pango.Tags.weight_to_int w))
  | #cell_properties as x -> cell_renderer_param x
  | #cell_properties_text_only as x -> cell_renderer_text_param x
let cell_renderer_toggle_param' = function
  | #cell_properties_toggle_only as x -> cell_renderer_toggle_param x
  | #cell_properties as x -> cell_renderer_param x
let cell_renderer_progress_param' = function
  | #cell_properties_progress_only as x -> cell_renderer_progress_param x
  | #cell_properties as x -> cell_renderer_param x
let cell_renderer_combo_param' = function
  | `MODEL None -> Gobject.param CellRendererCombo.P.model None
  | `MODEL (Some m : model option) -> Gobject.param CellRendererCombo.P.model (Some m#as_model)
  | `TEXT_COLUMN c -> Gobject.param CellRendererCombo.P.text_column c.index
  | `HAS_ENTRY b -> Gobject.param CellRendererCombo.P.has_entry b
  | #cell_properties_text as x -> cell_renderer_text_param' x

let cell_renderer_accel_param' = function
  | `KEYCODE i ->  Gobject.param CellRendererAccel.P.keycode i
  | `KEY i  -> Gobject.param CellRendererAccel.P.accel_key i
  | `ACCEL_MODE m  -> Gobject.param CellRendererAccel.P.accel_mode m
  | `MODS m  -> 
      Gobject.param 
	CellRendererAccel.P.accel_mods 
	(Gpointer.encode_flags GdkEnums.modifier m);
  | #cell_properties_text as x -> cell_renderer_text_param' x

class type ['a, 'b] cell_renderer_skel =
  object
    inherit gtkobj
    val obj : 'a obj
    method as_renderer : Gtk.cell_renderer obj
    method get_property : ('a, 'c) property -> 'c
    method set_properties : 'b list -> unit
  end

class virtual ['a,'b] cell_renderer_impl obj = object (self)
  inherit gtkobj obj
  method as_renderer = (obj :> Gtk.cell_renderer obj)
  method private virtual param : 'b -> 'a param
  method set_properties l = set_params obj (List.map ~f:self#param l)
  method get_property : 'c. ('a,'c) property -> 'c = Gobject.Property.get obj
end

class cell_renderer_pixbuf obj = object
  inherit [Gtk.cell_renderer_pixbuf,cell_properties_pixbuf]
      cell_renderer_impl obj
  method private param = cell_renderer_pixbuf_param'
  method connect = new gtkobj_signals_impl obj
end

class cell_renderer_text_signals obj = object (self)
  inherit gtkobj_signals_impl (obj : [>Gtk.cell_renderer_text] obj)
  method edited = self#connect CellRendererText.S.edited
end

class cell_renderer_text obj = object
  inherit [Gtk.cell_renderer_text,cell_properties_text] cell_renderer_impl obj
  method private param = cell_renderer_text_param'
  method set_fixed_height_from_font =
    CellRendererText.set_fixed_height_from_font obj
  method connect = new cell_renderer_text_signals obj
end
class cell_renderer_toggle_signals obj = object (self)
  inherit gtkobj_signals_impl (obj : Gtk.cell_renderer_toggle obj)
  method toggled = self#connect CellRendererToggle.S.toggled
end
class cell_renderer_toggle obj = object
  inherit [Gtk.cell_renderer_toggle,cell_properties_toggle]
      cell_renderer_impl obj
  method private param = cell_renderer_toggle_param'
  method connect = new cell_renderer_toggle_signals obj
end

class cell_renderer_progress obj = object
  inherit [Gtk.cell_renderer_progress,cell_properties_progress]
      cell_renderer_impl obj
  method private param = cell_renderer_progress_param'
  method connect = new gtkobj_signals_impl obj
end

class cell_renderer_combo_signals obj = object (self)
  inherit cell_renderer_text_signals obj
  method changed = self#connect CellRendererCombo.S.changed
end

class cell_renderer_combo obj = object
  inherit [Gtk.cell_renderer_combo,cell_properties_combo]
      cell_renderer_impl obj
  method private param = cell_renderer_combo_param'
  method set_fixed_height_from_font =
    CellRendererText.set_fixed_height_from_font obj
  method connect =
    new cell_renderer_combo_signals (obj :> Gtk.cell_renderer_combo Gtk.obj)
end

class cell_renderer_accel_signals (obj:Gtk.cell_renderer_accel Gtk.obj) = 
object(self)
  inherit gtkobj_signals_impl obj
  method edited = self#connect CellRendererText.S.edited
  method accel_edited = self#connect CellRendererAccel.S.accel_edited
  method accel_cleared = self#connect CellRendererAccel.S.accel_cleared
end

class cell_renderer_accel obj = object
  inherit [Gtk.cell_renderer_accel,cell_properties_accel]
      cell_renderer_impl obj
  method private param = cell_renderer_accel_param'
  method connect = new cell_renderer_accel_signals obj
end

let cell_renderer_pixbuf l =
  new cell_renderer_pixbuf
    (CellRendererPixbuf.create (List.map cell_renderer_pixbuf_param' l))
let cell_renderer_text l =
  new cell_renderer_text
    (CellRendererText.create (List.map cell_renderer_text_param' l))
let cell_renderer_toggle l =
  new cell_renderer_toggle
    (CellRendererToggle.create (List.map cell_renderer_toggle_param' l))
let cell_renderer_progress l =
  new cell_renderer_progress
    (CellRendererProgress.create (List.map cell_renderer_progress_param' l))
let cell_renderer_combo l =
  new cell_renderer_combo
    (CellRendererCombo.create (List.map cell_renderer_combo_param' l))
let cell_renderer_accel (l:cell_properties_accel list) =
  new cell_renderer_accel
    (CellRendererAccel.create (List.map cell_renderer_accel_param' l))


class icon_view_signals obj = object (self)
  inherit container_signals_impl obj
  inherit OgtkTreeProps.icon_view_sigs
end

class icon_view obj = object
  inherit [[> Gtk.icon_view]] GContainer.container_impl obj
  inherit OgtkTreeProps.icon_view_props

  method connect = new icon_view_signals obj
  method event = new GObj.event_ops obj

  method model =
    new model (Gobject.Property.get_some obj IconView.P.model)
  method set_model (m : model option) =
    Gobject.set IconView.P.model obj (Gaux.may_map (fun m -> m#as_model) m)
  method set_markup_column (c : string column) =
    Gobject.set IconView.P.markup_column obj c.index
  method set_text_column (c : string column) =
    Gobject.set IconView.P.text_column obj c.index
  method set_pixbuf_column (c : GdkPixbuf.pixbuf column) =
    Gobject.set IconView.P.pixbuf_column obj c.index

  method get_path_at_pos = IconView.get_path_at_pos obj
  method selected_foreach = IconView.selected_foreach obj
  method select_path = IconView.select_path obj
  method unselect_path = IconView.unselect_path obj
  method path_is_selected = IconView.path_is_selected obj
  method get_selected_items = IconView.get_selected_items obj
  method select_all () = IconView.select_all obj
  method unselect_all () = IconView.unselect_all obj
  method item_activated = IconView.item_activated obj
end

let icon_view ?model =
  let model = Gaux.may_map (fun m -> m#as_model) model in
  IconView.make_params ?model [] ~cont:(
  GContainer.pack_container ~create:(fun p -> new icon_view (IconView.create p)))

(* Custom models *)

class type virtual ['obj,'row,'a,'b,'c] custom_tree_model_type = 
object
  inherit model
  val obj : 'obj
  val n_columns : int
  val columns : Gobject.g_type array
  method custom_n_columns : int
  method custom_get_column_type : int -> Gobject.g_type

  method connect : model_signals
    
  (** Signal emitters *)
  method custom_row_changed : Gtk.tree_path -> 'row -> unit
  method custom_row_deleted : Gtk.tree_path -> unit
  method custom_row_has_child_toggled :
    Gtk.tree_path -> 'row -> unit
  method custom_row_inserted : Gtk.tree_path -> 'row -> unit
  method custom_rows_reordered :
    Gtk.tree_path -> 'row option -> int array -> unit

  method custom_unref_node : 'row -> unit
  method custom_ref_node : 'row -> unit

  method custom_flags : GtkEnums.tree_model_flags list

  method virtual custom_get_iter : Gtk.tree_path -> 'row option
  method virtual custom_get_path : 'row -> Gtk.tree_path
  method custom_get_value :
    'row -> int -> Gobject.g_value -> unit

  method virtual custom_value : 'a. Gobject.g_type -> 'row -> column:int -> Gobject.basic
  method virtual custom_iter_children : 'row option -> 'row option
  method virtual custom_iter_has_child : 'row -> bool
  method virtual custom_iter_n_children : 'row option -> int
  method virtual custom_iter_next : 'row -> 'row option
  method virtual custom_iter_nth_child : 'row option -> int -> 'row option
  method virtual custom_iter_parent : 'row -> 'row option

  method virtual custom_decode_iter : 'a -> 'b -> 'c -> 'row
  method virtual custom_encode_iter : 'row -> 'a * 'b * 'c

end

class virtual ['row,'a,'b,'c] custom_tree_model (column_list:column_list) = 
  let obj = (GtkTree.CustomModel.create ()) in
object (self)
  inherit model obj
  method connect = new model_signals obj  

  inherit ['row,'a,'b,'c] GtkTree.CustomModel.callback
  val n_columns =  List.length column_list#types
  val columns = Array.of_list column_list#types
  
  method get ~row:_ ~column:_ = failwith "get not allowed on a custom model."

  method custom_n_columns = n_columns

  method custom_get_value (row:'row) (column:int) (value:Gobject.g_value) =
    Gobject.Value.init value (columns.(column));
    if column >=0 && column <n_columns then
      let (#basic as value_to_set) = self#custom_value columns.(column) row ~column in
      try 
        Gobject.Value.set value value_to_set
      with Failure _ -> 
        failwith 
          ("custom_value returned a value of incompatible type for column "^string_of_int column
           ^" of type "^ (Gobject.Type.name (Gobject.Value.get_type value)))
    else invalid_arg ("custom_get_value: invalid column id "^string_of_int column)

  method virtual custom_value : 'a. Gobject.g_type -> 'row -> column:int -> Gobject.basic

  method custom_get_column_type n : Gobject.g_type = 
    if 0 <= n && n < n_columns then columns.(n)
    else Gobject.Type.of_fundamental `INVALID

  method custom_row_inserted path (iter:'row) =
    CustomModel.custom_row_inserted obj path iter
  method custom_row_changed path (iter:'row) =
    CustomModel.custom_row_changed obj path iter
  method custom_row_has_child_toggled path (iter:'row) =
    CustomModel.custom_row_has_child_toggled obj path iter
  method custom_row_deleted (path:Gtk.tree_path) =
    CustomModel.custom_row_deleted obj path
  method custom_rows_reordered path (iter_opt:'row option) new_order =
    CustomModel.custom_rows_reordered obj path iter_opt new_order
  method custom_flags : GtkEnums.tree_model_flags list = []
  initializer 
    GtkTree.CustomModel.register_callback obj self;
    column_list#lock ();
    let id = Gobject.get_oid obj in
    Hashtbl.add model_ids id column_list#id;
    Hashtbl.add custom_model_ids column_list#id ();
    (* Invalidate all iterators before dying...*)
    Gc.finalise (fun m -> m#foreach (fun p _ -> m#custom_row_deleted p; false)) self

end
