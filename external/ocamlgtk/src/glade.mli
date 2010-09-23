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

(* $Id: glade.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

(* This module provides some low-level interfacing with libglade *)

external init : unit -> unit = "ml_glade_init"
    (* You must call [init] before importing any glade specification *)
    (* Returns immediately if already initialized *)

(* The raw glade XML widget *)
type glade_xml = [ `data | `glade_xml]

val create :
    ?file:string -> ?data:string ->
    ?root:string -> ?domain:string -> unit -> glade_xml Gtk.obj
      (* One of [file] or [data] must be given, [data] is preferred when *)
      (* both are given. If [root] is omitted the first widget is used *)
      (* as root. [domain] is for localization. *)

val signal_autoconnect :
  [> `glade_xml] Gtk.obj ->
  f:(handler:string ->
     signal:string ->
     after:bool -> ?target:unit Gtk.obj -> unit Gtk.obj -> unit) ->
  unit
external get_widget :
  [> `glade_xml] Gtk.obj -> name:string -> Gtk.widget Gtk.obj
  = "ml_glade_xml_get_widget"
external get_widget_name : [> `widget] Gtk.obj -> string
  = "ml_glade_get_widget_name"
external get_widget_tree : [> `widget] Gtk.obj -> glade_xml Gtk.obj
  = "ml_glade_get_widget_tree"

val get_widget_msg :
    name:string -> ?info:string -> [> `glade_xml] Gtk.obj -> Gtk.widget Gtk.obj
      (* Same as get_widget, but fails with a useful message including info,
         rather than just raising Gpointer.Null *)

(* Handler bindings *)
type handler =
  [ `Simple of unit -> unit
  | `Object of string * (unit Gtk.obj -> unit)
  | `Custom of Gobject.Closure.argv -> Gobject.data_get list -> unit]
val gtk_bool : bool -> Gobject.Closure.argv -> 'a -> unit

val add_handler : name:string -> handler -> unit
    (* Add a global handler for some well known name.
       The default ones (gtk_main_quit, gtk_widget_destroy, ...) are
       already defined. *)
val bind_handlers :
  ?extra:(string * handler) list ->
  ?warn:bool -> [> `glade_xml] Gtk.obj -> unit
    (* Bind handlers on a glade widget. You may add some local bindings
       specific to this widget. Warn for missing handlers. *)

val bind_handler :
  name:string -> handler:handler ->
  ?warn:bool -> [> `glade_xml] Gtk.obj -> unit
    (* Bind an individual handler. Warn if unused. *)

val print_bindings : out_channel -> [> `glade_xml] Gtk.obj -> unit
    (* List all the bindings in a xml widget *)

val trace_handlers : out_channel -> [> `glade_xml] Gtk.obj -> unit
    (* trace calls to glade handlers *)

(* Class skeleton, for use in generated wrappers *)

class xml : ?trace:out_channel -> ?autoconnect:bool -> glade_xml Gtk.obj ->
  object
    val xml : glade_xml Gtk.obj
    method xml : glade_xml Gtk.obj
    method bind : name:string -> callback:(unit -> unit) -> unit
  end
  (* wrap a glade_xml widget, and run signal_autoconnect (default) *)
