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

(* $Id: gtkBroken.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gtk
open Tags
open GtkBrokenProps
open GtkBase

external _gtkbroken_init : unit -> unit = "ml_gtkbroken_init"
let () = _gtkbroken_init ()

module TreeItem = struct
  include TreeItem
  external create_with_label : string -> tree_item obj
      = "ml_gtk_tree_item_new_with_label"
  let create ?label () =
    match label with None -> create []
    | Some label -> create_with_label label
  external subtree : [>`treeitem] obj -> tree obj
      = "ml_GTK_TREE_ITEM_SUBTREE"
end

module Tree = struct
  include Tree
  external insert : [>`tree] obj -> [>`treeitem] obj -> pos:int -> unit
      = "ml_gtk_tree_insert"
  external remove_items : [>`tree] obj -> [>`treeitem] obj list -> unit
      = "ml_gtk_tree_remove_items"
  external clear_items : [>`tree] obj -> start:int -> stop:int -> unit
      = "ml_gtk_tree_clear_items"
  external select_item : [>`tree] obj -> pos:int -> unit
      = "ml_gtk_tree_select_item"
  external unselect_item : [>`tree] obj -> pos:int -> unit
      = "ml_gtk_tree_unselect_item"
  external child_position : [>`tree] obj -> [>`treeitem] obj -> int
      = "ml_gtk_tree_child_position"
  external set_selection_mode : [>`tree] obj -> selection_mode -> unit
      = "ml_gtk_tree_set_selection_mode"
  external set_view_mode : [>`tree] obj -> [`LINE|`ITEM] -> unit
      = "ml_gtk_tree_set_view_mode"
  external set_view_lines : [>`tree] obj -> bool -> unit
      = "ml_gtk_tree_set_view_lines"
  external selection : [>`tree] obj -> tree_item obj list =
    "ml_gtk_tree_selection"
  let set ?selection_mode ?view_mode ?view_lines w =
    let may_set f = may ~f:(f w) in
    may_set set_selection_mode selection_mode;
    may_set set_view_mode view_mode;
    may_set set_view_lines view_lines
end

module OldEditable = OldEditable

module Text = struct
  include Text
  external set_point : [>`text] obj -> int -> unit
      = "ml_gtk_text_set_point"
  external get_point : [>`text] obj -> int = "ml_gtk_text_get_point"
  external get_length : [>`text] obj -> int = "ml_gtk_text_get_length"
  external freeze : [>`text] obj -> unit = "ml_gtk_text_freeze"
  external thaw : [>`text] obj -> unit = "ml_gtk_text_thaw"
  external insert :
      [>`text] obj -> ?font:Gdk.font -> ?foreground:Gdk.color ->
      ?background:Gdk.color -> string -> unit
      = "ml_gtk_text_insert"
  external forward_delete: [>`text] obj -> int -> unit = 
    "ml_gtk_text_forward_delete"
  external backward_delete: [>`text] obj -> int -> unit = 
    "ml_gtk_text_backward_delete"
end
