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

(* $Id: gList.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gtk
open GObj
open GContainer

(** Widget for packing a list of selectable items *)

(** {3 GtkListItem} *)

(** An item in a {!GList.liste}
   @gtkdoc gtk GtkListItem 
   @deprecated . *)
class list_item : Gtk.list_item obj ->
  object
    inherit GContainer.container
    val obj : Gtk.list_item obj
    method event : event_ops
    method as_item : Gtk.list_item obj
    method connect : item_signals
    method deselect : unit -> unit
    method select : unit -> unit
    method toggle : unit -> unit
  end

(** @gtkdoc gtk GtkListItem
   @deprecated . *)
val list_item :
  ?label:string ->
  ?packing:(list_item -> unit) -> ?show:bool -> unit -> list_item

(** {3 GtkList} *)

(** @gtkdoc gtk GtkList *)
class liste_signals : Gtk.liste obj ->
  object
    inherit GContainer.container_signals
    val obj : Gtk.liste obj
    method select_child : callback:(list_item -> unit) -> GtkSignal.id
    method selection_changed : callback:(unit -> unit) -> GtkSignal.id
    method unselect_child : callback:(list_item -> unit) -> GtkSignal.id
  end

(** Widget for packing a list of selectable items
   @gtkdoc gtk GtkList
   @deprecated . *)
class liste : Gtk.liste obj ->
  object
    inherit [list_item] GContainer.item_container
    val obj : Gtk.liste obj
    method child_position : list_item -> int
    method clear_items : start:int -> stop:int -> unit
    method connect : liste_signals
    method insert : list_item -> pos:int -> unit
    method select_item : pos:int -> unit
    method unselect_item : pos:int -> unit
    method private wrap : Gtk.widget obj -> list_item
    method set_selection_mode : Tags.selection_mode -> unit
    method selection_mode : Tags.selection_mode
  end

(** @gtkdoc gtk GtkList
   @deprecated . *)
val liste :
  ?selection_mode:Tags.selection_mode ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> liste

(** {3 GtkCList} *)

(** @gtkdoc gtk GtkCList *)
class clist_signals : 'a obj ->
  object
    inherit GContainer.container_signals
    constraint 'a = [> clist]
    val obj : 'a obj
    method click_column : callback:(int -> unit) -> GtkSignal.id
    method resize_column : callback:(int -> int -> unit) -> GtkSignal.id
    method select_all : callback:(unit -> unit) -> GtkSignal.id
    method unselect_all : callback:(unit -> unit) -> GtkSignal.id
    method select_row :
      callback:(row:int ->
                column:int -> event:GdkEvent.Button.t option -> unit) ->
      GtkSignal.id
    method unselect_row :
      callback:(row:int ->
                column:int -> event:GdkEvent.Button.t option -> unit) ->
      GtkSignal.id
    method scroll_horizontal :
      callback:(Tags.scroll_type -> pos:clampf -> unit) -> GtkSignal.id
    method scroll_vertical :
      callback:(Tags.scroll_type -> pos:clampf -> unit) -> GtkSignal.id
  end

(** A multi-columned scrolling list widget
   @gtkdoc gtk GtkCList
   @deprecated . *)
class ['a] clist : Gtk.clist obj ->
  object
    inherit GObj.widget
    val obj : Gtk.clist obj
    method event : event_ops
    method append : string list -> int
    method cell_pixmap : int -> int -> GDraw.pixmap option
    method cell_style : int -> int -> style option
    method cell_text : int -> int -> string
    method cell_type : int -> int -> Tags.cell_type
    method clear : unit -> unit
    method column_title : int -> string
    method column_widget : int -> widget
    method columns : int
    method columns_autosize : unit -> unit
    method connect : clist_signals
    method focus_row : int
    method freeze : unit -> unit
    method get_row_column : x:int -> y:int -> int * int
    method get_row_data : int -> 'a
    method hadjustment : GData.adjustment
    method insert : row:int -> string list -> int
    method moveto :
      ?row_align:clampf -> ?col_align:clampf -> int -> int -> unit
    method optimal_column_width : int -> int
    method prepend : string list -> int
    method remove : row:int -> unit
    method row_is_visible : int -> Tags.visibility
    method row_move : int -> dst:int -> unit
    method row_selectable : int -> bool
    method row_style : int -> style option
    method rows : int
    method scroll_vertical : Tags.scroll_type -> pos:clampf -> unit
    method scroll_horizontal : Tags.scroll_type -> pos:clampf -> unit
    method select : int -> int -> unit
    method select_all : unit -> unit
    method set_border_width : int -> unit
    method set_button_actions : int -> Tags.button_action list -> unit
    method set_cell :
      ?text:string ->
      ?pixmap:GDraw.pixmap ->
      ?spacing:int -> ?style:style -> int -> int -> unit
    method set_column :
      ?widget:widget ->
      ?title:string ->
      ?title_active:bool ->
      ?justification:Tags.justification ->
      ?visibility:bool ->
      ?resizeable:bool ->
      ?auto_resize:bool ->
      ?width:int -> ?min_width:int -> ?max_width:int -> int -> unit
    method set_hadjustment : GData.adjustment -> unit
    method set_reorderable : bool -> unit
    method set_row :
      ?foreground:GDraw.optcolor ->
      ?background:GDraw.optcolor ->
      ?selectable:bool ->
      ?style:style -> int -> unit
    method set_row_data : int -> data:'a -> unit
    method set_row_height : int -> unit
    method set_selection_mode : Tags.selection_mode -> unit
    method set_shadow_type : Tags.shadow_type -> unit
    method set_shift : int -> int -> vertical:int -> horizontal:int -> unit
    method set_sort :
      ?auto:bool -> ?column:int -> ?dir:Tags.sort_type -> unit -> unit
    method set_titles_active : bool -> unit
    method set_titles_show : bool -> unit
    method set_use_drag_icons : bool -> unit
    method set_vadjustment : GData.adjustment -> unit
    method sort : unit -> unit
    method swap_rows : int -> int -> unit
    method thaw : unit -> unit
    method unselect : int -> int -> unit
    method unselect_all : unit -> unit
    method vadjustment : GData.adjustment
    method get_row_state : int -> Gtk.Tags.state_type
  end

(** @gtkdoc gtk GtkCList 
   @deprecated . *)
val clist :
  ?columns:int ->
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?titles:string list ->
  ?button_actions:(int * Tags.button_action list) list ->
  ?titles_show:bool ->
  ?auto_sort:bool ->
  ?sort_column:int ->
  ?sort_type:Tags.sort_type ->
  ?reorderable:bool ->
  ?row_height:int ->
  ?selection_mode:Tags.selection_mode ->
  ?shadow_type:Tags.shadow_type ->
  ?titles_active:bool ->
  ?use_drag_icons:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> string clist
val clist_poly :
  ?columns:int ->
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?titles:string list ->
  ?button_actions:(int * Tags.button_action list) list ->
  ?titles_show:bool ->
  ?auto_sort:bool ->
  ?sort_column:int ->
  ?sort_type:Tags.sort_type ->
  ?reorderable:bool ->
  ?row_height:int ->
  ?selection_mode:Tags.selection_mode ->
  ?shadow_type:Tags.shadow_type ->
  ?titles_active:bool ->
  ?use_drag_icons:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> 'a clist
