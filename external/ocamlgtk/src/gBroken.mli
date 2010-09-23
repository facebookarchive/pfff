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

(* $Id: gBroken.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gobject
open Gtk
open GObj
open GContainer

(** Deprecated widgets
   @gtkdoc gtk Deprecated *)

(** {3 Obsolete GtkTree/GtkTreeItem framework} *)

(** @gtkdoc gtk GtkTreeItem
    @deprecated use {!GTree.view} instead *)
class tree_item_signals : tree_item obj ->
  object
    inherit GContainer.item_signals
    method collapse : callback:(unit -> unit) -> GtkSignal.id
    method expand : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkTreeItem
    @deprecated use {!GTree.view} instead *)
class tree_item : Gtk.tree_item obj ->
  object
    inherit GContainer.container
    val obj : Gtk.tree_item obj
    method event : event_ops
    method as_item : Gtk.tree_item obj
    method collapse : unit -> unit
    method connect : tree_item_signals
    method expand : unit -> unit
    method remove_subtree : unit -> unit
    method set_subtree : tree -> unit
    method subtree : tree option
  end

(** @gtkdoc gtk GtkTree 
    @deprecated use {!GTree.view} instead *)
and tree_signals : Gtk.tree obj ->
  object
    inherit GContainer.container_signals
    val obj : Gtk.tree obj
    method select_child : callback:(tree_item -> unit) -> GtkSignal.id
    method selection_changed : callback:(unit -> unit) -> GtkSignal.id
    method unselect_child : callback:(tree_item -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkTree 
    @deprecated use {!GTree.view} instead *)
and tree : Gtk.tree obj ->
  object
    inherit [tree_item] GContainer.item_container
    val obj : Gtk.tree obj
    method event : event_ops
    method as_tree : Gtk.tree obj
    method child_position : tree_item -> int
    method clear_items : start:int -> stop:int -> unit
    method connect : tree_signals
    method insert : tree_item -> pos:int -> unit
    method remove_items : tree_item list -> unit
    method select_item : pos:int -> unit
    method selection : tree_item list
    method set_selection_mode : Tags.selection_mode -> unit
    method set_view_lines : bool -> unit
    method set_view_mode : [`LINE|`ITEM] -> unit
    method unselect_item : pos:int -> unit
    method private wrap : Gtk.widget obj -> tree_item
  end

(** @gtkdoc gtk GtkTreeItem
    @deprecated use {!GTree.view} instead *)
val tree_item :
  ?label:string ->
  ?packing:(tree_item -> unit) -> ?show:bool -> unit -> tree_item

(** @gtkdoc gtk GtkTree 
    @deprecated use {!GTree.view} instead *)
val tree :
  ?selection_mode:Tags.selection_mode ->
  ?view_mode:[`LINE|`ITEM] ->
  ?view_lines:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int -> ?packing:(widget -> unit) -> ?show:bool -> unit -> tree

(** {3 Obsolete GtkOldEditable/GtkText framework} *)

class old_editable_signals : ([> Gtk.old_editable ] as 'b) Gtk.obj ->
  object
    inherit GEdit.editable_signals
    val obj : 'b Gtk.obj
    method activate : callback:(unit -> unit) -> GtkSignal.id
    method copy_clipboard : callback:(unit -> unit) -> GtkSignal.id
    method cut_clipboard : callback:(unit -> unit) -> GtkSignal.id
    method move_cursor : callback:(int -> int -> unit) -> GtkSignal.id
    method move_page : callback:(int -> unit) -> GtkSignal.id
    method move_to_column : callback:(int -> unit) -> GtkSignal.id
    method move_to_row : callback:(int -> unit) -> GtkSignal.id
    method move_word : callback:(int -> unit) -> GtkSignal.id
    method paste_clipboard : callback:(unit -> unit) -> GtkSignal.id
 end

class text : Gtk.text Gtk.obj ->
  object
    inherit GEdit.editable
    inherit [Gtk.text] GObj.objvar
    method connect : old_editable_signals
    method backward_delete : int -> unit
    method event : GObj.event_ops
    method forward_delete : int -> unit
    method freeze : unit -> unit
    method hadjustment : GData.adjustment
    method insert :
      ?font:Gdk.font ->
      ?foreground:GDraw.color -> ?background:GDraw.color -> string -> unit
    method length : int
    method line_wrap : bool
    method point : int
    method set_hadjustment : GData.adjustment -> unit
    method set_line_wrap : bool -> unit
    method set_point : int -> unit
    method set_vadjustment : GData.adjustment -> unit
    method set_word_wrap : bool -> unit
    method thaw : unit -> unit
    method vadjustment : GData.adjustment
    method word_wrap : bool
 end

val text :
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?editable: bool ->
  ?line_wrap:bool -> ?word_wrap:bool ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> text
