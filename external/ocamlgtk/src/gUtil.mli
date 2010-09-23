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

(* $Id: gUtil.mli 1393 2008-01-18 01:17:21Z garrigue $ *)

(** {3 Utility classes for programming with GTK objects} *)

open GObj

(** A nice function to use with [#install_printer] *)
val print_widget : Format.formatter -> #widget -> unit

(** The memo class provides an easy way to remember the real class of
   a widget.
   Insert all widgets of class in one single [t memo], and you can then
   recover their original ML object with [#find].
*)

class ['a] memo : unit ->
  object
    constraint 'a = <get_oid: int; ..>
    val tbl : (int, 'a) Hashtbl.t
    method add : 'a -> unit
    method find : widget -> 'a
    method remove : widget -> unit
  end

(** {4 The ML signal mechanism}
   It allows one to add GTK-like signals to arbitrary objects.
*)

val next_callback_id : unit -> GtkSignal.id

class ['a] signal :
  unit ->
  object
    val mutable callbacks : (GtkSignal.id * ('a -> unit)) list
    method callbacks : (GtkSignal.id * ('a -> unit)) list
    method call : 'a -> unit
    method connect : after:bool -> callback:('a -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> bool
  end

(** As with GTK signals, you can use [GtkSignal.stop_emit] inside a
   callback to prevent other callbacks from being called. *)

class virtual ml_signals : (GtkSignal.id -> bool) list ->
  object ('a)
    val after : bool
    method after : 'a
    method disconnect : GtkSignal.id -> unit
    val mutable disconnectors : (GtkSignal.id -> bool) list
  end
class virtual add_ml_signals :
  'a Gtk.obj -> (GtkSignal.id -> bool) list ->
  object
    method disconnect : GtkSignal.id -> unit
    val mutable disconnectors : (GtkSignal.id -> bool) list
  end

(** To add ML signals to a LablGTK object:
{[
   class mywidget_signals obj ~mysignal1 ~mysignal2 = object
     inherit somewidget_signals obj
     inherit add_ml_signals obj [mysignal1#disconnect; mysignal2#disconnect]
     method mysignal1 = mysignal1#connect ~after
     method mysignal2 = mysignal2#connect ~after
   end

   class mywidget obj = object (self)
     inherit somewidget obj
     val mysignal1 = new signal obj
     val mysignal2 = new signal obj
     method connect = new mywidget_signals obj ~mysignal1 ~mysignal2
     method call1 = mysignal1#call
     method call2 = mysignal2#call
   end
]}
   You can also add ML signals to an arbitrary object; just inherit
   from [ml_signals] in place of [widget_signals]+[add_ml_signals].
{[ 
  class mysignals ~mysignal1 ~mysignal2 = object
     inherit ml_signals [mysignal1#disconnect; mysignal2#disconnect]
     method mysignal1 = mysignal1#connect ~after
     method mysignal2 = mysignal2#connect ~after
   end
]}
*)

(** {4 Propagating state modifications}
   The variable class provides an easy way to propagate state modifications.
   A new variable is created by [new variable init]. The [#set] method just
   calls the [set] signal, which by default only calls [real_set].
   [real_set] sets the variable and calls [changed] when needed.
   Deep equality is used to compare values, but check is only done if
   there are callbacks for [changed].
*)

class ['a] variable_signals :
  set:'a signal -> changed:'a signal ->
  object ('b)
    val after : bool
    method after : 'b
    method set : callback:('a -> unit) -> GtkSignal.id
    method changed : callback:('a -> unit) -> GtkSignal.id
    method disconnect : GtkSignal.id -> unit
    val mutable disconnectors : (GtkSignal.id -> bool) list
  end

class ['a] variable : 'a ->
  object
    val set : 'a signal
    val changed : 'a signal
    val mutable x : 'a
    method connect : 'a variable_signals
    method get : 'a
    method set : 'a -> unit
    method private equal : 'a -> 'a -> bool
    method private real_set : 'a -> unit
  end
