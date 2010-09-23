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

(* $Id: gContainer.ml 1499 2010-04-08 08:00:42Z garrigue $ *)

open StdLabels
open Gaux
open Gobject
open Gtk
open GtkBase
open OgtkBaseProps
open GObj
open GData

open Container

class focus obj = object
  val obj = obj
  (* method circulate = focus obj *)
  method set (child : widget option) =
    let child = may_map child ~f:(fun x -> x#as_widget) in
    set_focus_child obj (Gpointer.optboxed child)
  method set_hadjustment adj =
    set_focus_hadjustment obj
      (Gpointer.optboxed (may_map adj ~f:as_adjustment))
  method set_vadjustment adj =
    set_focus_vadjustment obj
      (Gpointer.optboxed (may_map adj ~f:as_adjustment))
end

class ['a] container_impl obj = object (self)
  inherit ['a] widget_impl obj
  inherit container_props
  method add w = add obj (as_widget w)
  method remove w = remove obj (as_widget w)
  method children = List.map ~f:(new widget) (children obj)
  method all_children =
    let l = ref [] in
    forall obj ~f:(fun w -> l := new widget w :: !l);
    List.rev !l
  method focus = new focus obj
end

class container = ['a] container_impl

class container_signals_impl obj = object
  inherit widget_signals_impl obj
  inherit container_sigs
end

class type container_signals = container_signals_impl

class container_full obj = object
  inherit container obj
  method connect = new container_signals_impl obj
end

let cast_container (w : widget) =
  new container_full (cast w#as_widget)

let pack_container ~create =
  Container.make_params ~cont:
    (fun p ?packing ?show () -> pack_return (create p) ~packing ~show)

class ['a] bin_impl obj = object
  inherit ['a] container_impl obj
  method child = new widget (Bin.get_child obj)
end

class bin = ['a] bin_impl

class virtual ['a] item_container obj = object (self)
  inherit ['b] widget_impl obj
  inherit container_props
  method add (w : 'a) =
    add obj w#as_item
  method remove (w : 'a) =
    remove obj w#as_item
  method private virtual wrap : Gtk.widget obj -> 'a
  method children : 'a list =
    List.map ~f:self#wrap (children obj)
  method all_children =
    let l = ref [] in
    forall obj ~f:(fun w -> l := self#wrap w :: !l);
    List.rev !l
  method focus = new focus obj
  method virtual insert : 'a -> pos:int -> unit
  method append (w : 'a) = self#insert w ~pos:(-1)
  method prepend (w : 'a) = self#insert w ~pos:0
end

class item_signals obj = object
  inherit container_signals_impl (obj : [> Gtk.item] obj)
  inherit item_sigs
end
