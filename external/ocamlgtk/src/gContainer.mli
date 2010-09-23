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

(* $Id: gContainer.mli 1397 2008-02-26 00:21:39Z garrigue $ *)

open Gtk
open GObj

(** Widgets which contain other widgets *)

class focus :
  'a obj ->
  object
    constraint 'a = [> `container]
    val obj : 'a obj
    (* method circulate : Tags.direction_type -> bool *)
    method set : widget option -> unit
    method set_hadjustment : GData.adjustment option -> unit
    method set_vadjustment : GData.adjustment option -> unit
  end

(** {3 GtkContainer} *)

(** Base class for widgets which contain other widgets
   @gtkdoc gtk GtkContainer *)
class container : ([> Gtk.container] as 'a) obj ->
  object
    inherit GObj.widget
    val obj : 'a obj
    method add : widget -> unit
    method children : widget list     (* using foreach *)
    method all_children : widget list (* using forall *)
    method remove : widget -> unit
    method focus : focus
    method set_border_width : int -> unit
    method set_resize_mode : Tags.resize_mode -> unit
    method border_width : int
    method resize_mode : Tags.resize_mode
  end

(** @gtkdoc gtk GtkContainer *)
class ['a] container_impl :([> Gtk.container] as 'a) obj ->
  object
    inherit container
    inherit ['a] GObj.objvar
  end

(** @gtkdoc gtk GtkContainer *)
class type container_signals =
  object
    inherit GObj.widget_signals
    method add : callback:(widget -> unit) -> GtkSignal.id
    method remove : callback:(widget -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkContainer *)
class container_signals_impl : ([> Gtk.container] as 'a) obj ->
  object
    inherit ['a] GObj.gobject_signals
    inherit container_signals
  end

(** @gtkdoc gtk GtkContainer *)
class container_full : ([> Gtk.container] as 'a) obj ->
  object
    inherit container
    val obj : 'a obj
    method connect : container_signals
  end

(** @raise Gtk.Cannot_cast "GtkContainer" *)
val cast_container : widget -> container_full

(** @gtkdoc gtk GtkContainer *)
val pack_container :
  create:(([> Gtk.container] as 'a) Gobject.param list ->
          (#GObj.widget as 'b)) ->
  'a Gobject.param list ->
  ?border_width:int ->
  ?width:int ->
  ?height:int -> ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> 'b

(** {3 GtkBin} *)

(** @gtkdoc gtk GtkBin *)
class bin : ([> Gtk.bin] as 'a) obj ->
  object
    inherit container
    val obj : 'a obj
    method child : widget
    (** @raise Gpointer.Null if the widget has no child. *)
  end

(** @gtkdoc gtk GtkBin *)
class ['a] bin_impl :([> Gtk.bin] as 'a) obj ->
  object
    inherit bin
    inherit ['a] GObj.objvar
  end

(** {3 GtkItem} *)

(** @gtkdoc gtk GtkContainer *)
class virtual ['a] item_container : ([> Gtk.container] as 'c) obj ->
  object
    constraint 'a = < as_item : [>`widget] obj; .. >
    inherit GObj.widget
    val obj : 'c obj
    method add : 'a -> unit
    method append : 'a -> unit
    method children : 'a list
    method all_children : 'a list
    method virtual insert : 'a -> pos:int -> unit
    method prepend : 'a -> unit
    method remove : 'a -> unit
    method focus : focus
    method set_border_width : int -> unit
    method set_resize_mode : Tags.resize_mode -> unit
    method border_width : int
    method resize_mode : Tags.resize_mode
    method private virtual wrap : Gtk.widget obj -> 'a
  end

(** @gtkdoc gtk GtkItem *)
class item_signals : [> Gtk.item] obj ->
  object
    inherit container_signals
    method deselect : callback:(unit -> unit) -> GtkSignal.id
    method select : callback:(unit -> unit) -> GtkSignal.id
    method toggle : callback:(unit -> unit) -> GtkSignal.id
  end
