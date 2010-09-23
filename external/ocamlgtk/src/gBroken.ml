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

(* $Id: gBroken.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels
open Gaux
open Gobject
open Gtk
open GtkBase
open GtkBroken
open OgtkBaseProps
open OgtkBrokenProps
open GObj
open GContainer

(* Obsolete GtkTree/GtkTreeItem framework *)

class tree_item_signals obj = object
  inherit container_signals_impl (obj : tree_item obj)
  inherit item_sigs
  inherit tree_item_sigs
end

class tree_item obj = object
  inherit container obj
  method event = new GObj.event_ops obj
  method as_item : Gtk.tree_item obj = obj
  method connect = new tree_item_signals obj
  method set_subtree (w : tree) = TreeItem.set_subtree obj w#as_tree
  method remove_subtree () = TreeItem.remove_subtree obj
  method expand () = TreeItem.expand obj
  method collapse () = TreeItem.collapse obj
  method subtree =
    try Some(new tree (TreeItem.subtree obj)) with Gpointer.Null -> None
end

and tree_signals obj = object (self)
  inherit container_signals_impl obj
  method selection_changed = self#connect Tree.S.selection_changed
  method select_child ~callback =
    self#connect Tree.S.select_child
      ~callback:(fun w -> callback (new tree_item (TreeItem.cast w))) 
  method unselect_child ~callback =
    self#connect Tree.S.unselect_child
      ~callback:(fun w -> callback (new tree_item (TreeItem.cast w))) 
end

and tree obj = object (self)
  inherit [tree_item] item_container obj
  method event = new GObj.event_ops obj
  method as_tree = (obj :> Gtk.tree obj)
  method insert w ~pos = Tree.insert obj w#as_item ~pos
  method connect = new tree_signals obj
  method clear_items = Tree.clear_items obj
  method select_item = Tree.select_item obj
  method unselect_item = Tree.unselect_item obj
  method child_position (w : tree_item) = Tree.child_position obj w#as_item
  method remove_items items =
    Tree.remove_items obj
      (List.map ~f:(fun (t : tree_item) -> t#as_item) items)
  method set_selection_mode = Tree.set_selection_mode obj
  method set_view_mode = Tree.set_view_mode obj
  method set_view_lines = Tree.set_view_lines obj
  method selection =
    List.map ~f:(fun w -> self#wrap (w :> Gtk.widget obj)) (Tree.selection obj)
  method private wrap w =
    new tree_item (TreeItem.cast w)
end

let tree_item ?label ?packing ?show () =
  let w = TreeItem.create ?label () in
  let self = new tree_item w in
  may packing ~f:(fun f -> (f self : unit));
  if show <> Some false then self#misc#show ();
  self

let tree ?selection_mode ?view_mode ?view_lines =
  GContainer.pack_container [] ~create:(fun p ->
    let w = Tree.create p in
    Tree.set w ?selection_mode ?view_mode ?view_lines;
    new tree w)

(* Obsolete OldEditable / Text widget *)

class old_editable_signals obj = object
  inherit widget_signals_impl (obj : [>old_editable] obj)
  inherit OgtkEditProps.editable_sigs
  inherit old_editable_sigs
end

class text obj = object (self)
  inherit GEdit.editable (obj : Gtk.text obj) as super
  inherit text_props
  method connect = new old_editable_signals obj
  method event = new GObj.event_ops obj
  method get_chars ~start ~stop:e =
    if start < 0 || e > Text.get_length obj || e < start then
      invalid_arg "GBroken.text#get_chars";
    super#get_chars ~start ~stop:e
  method set_point = Text.set_point obj
  method point = Text.get_point obj
  method length = Text.get_length obj
  method freeze () = Text.freeze obj
  method thaw () = Text.thaw obj
  method insert ?font ?foreground ?background text =
    let colormap = try Some self#misc#colormap with _ -> None in
    Text.insert obj text ?font
      ?foreground:(may_map foreground ~f:(GDraw.color ?colormap))
      ?background:(may_map background ~f:(GDraw.color ?colormap))
  method forward_delete  = Text.forward_delete  obj
  method backward_delete = Text.backward_delete obj
end

let text ?hadjustment ?vadjustment =
  let hadjustment = may_map GData.as_adjustment hadjustment in
  let vadjustment = may_map GData.as_adjustment vadjustment in
  Text.make_params [] ?hadjustment ?vadjustment ~cont:
    (fun p ?packing ?show () ->
      pack_return (new text (Text.create p)) ~packing ~show)
