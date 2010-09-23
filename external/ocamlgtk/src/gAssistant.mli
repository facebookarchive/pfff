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

(* $Id: $ *)

open Gtk
open GObj

(** Assistants *)

(** {3 GtkAssistant} *)

(** @gtkdoc gtk GtkAssistant *)


(** @gtkdoc gtk GtkAssistant *)
class assistant_signals : ([> Gtk.assistant] as 'a) obj ->
object
  inherit GContainer.container_signals
  val obj : 'a obj
  method apply : callback:(unit -> unit) -> GtkSignal.id
  method cancel : callback:(unit -> unit) -> GtkSignal.id
  method close : callback:(unit -> unit) -> GtkSignal.id
  method leave : callback:(unit -> unit) -> GtkSignal.id
  method prepare : callback:(unit -> unit) -> GtkSignal.id
end

(** Toplevel widget which can contain other widgets in different pages
   @gtkdoc gtk GtkAssistant *)
class assistant : ([> Gtk.assistant] as 'a) obj ->
object
  inherit GWindow.window_skel
  val obj : 'a obj
  method connect : assistant_signals
  method set_current_page : int -> unit
  method add_action_widget : Gtk.widget Gtk.obj -> unit
  method append_page :
    ?page_type:GtkEnums.assistant_page_type ->
    ?title:string ->
    ?header_image:GdkPixbuf.pixbuf ->
    ?side_image:GdkPixbuf.pixbuf ->
    ?complete:bool -> Gtk.widget Gtk.obj -> int
  method current_page : int
  method insert_page :
    ?page_type:GtkEnums.assistant_page_type ->
    ?title:string ->
    ?header_image:GdkPixbuf.pixbuf ->
    ?side_image:GdkPixbuf.pixbuf ->
    ?complete:bool -> position:int -> Gtk.widget Gtk.obj -> int
  method n_pages : int
  method nth_page : int -> Gtk.widget Gtk.obj
  method page_complete : Gtk.widget Gtk.obj -> bool
  method page_header_image : Gtk.widget Gtk.obj -> GdkPixbuf.pixbuf
  method page_side_image : Gtk.widget Gtk.obj -> GdkPixbuf.pixbuf
  method page_title : Gtk.widget Gtk.obj -> string
  method page_type : Gtk.widget Gtk.obj -> GtkEnums.assistant_page_type
  method prepend_page :
    ?page_type:GtkEnums.assistant_page_type ->
    ?title:string ->
    ?header_image:GdkPixbuf.pixbuf ->
    ?side_image:GdkPixbuf.pixbuf ->
    ?complete:bool -> Gtk.widget Gtk.obj -> int
  method remove_action_widget : Gtk.widget Gtk.obj -> unit
  method set_page_complete : Gtk.widget Gtk.obj -> bool -> unit
  method set_page_header_image :
    Gtk.widget Gtk.obj -> GdkPixbuf.pixbuf -> unit
  method set_page_side_image :
    Gtk.widget Gtk.obj -> GdkPixbuf.pixbuf -> unit
  method set_page_title : Gtk.widget Gtk.obj -> string -> unit
  method set_page_type :
    Gtk.widget Gtk.obj -> GtkEnums.assistant_page_type -> unit
  method update_buttons_state : unit
end

(** @gtkdoc gtk GtkPlug *)
val assistant : ?title:string ->
  ?allow_grow:bool ->
  ?allow_shrink:bool ->
  ?decorated:bool ->
  ?deletable:bool ->
  ?focus_on_map:bool ->
  ?icon:GdkPixbuf.pixbuf ->
  ?icon_name:string ->
  ?modal:bool ->
  ?position:Tags.window_position ->
  ?resizable:bool ->
  ?screen:Gdk.screen ->
  ?type_hint:GdkEnums.window_type_hint ->
  ?urgency_hint:bool ->
  ?wm_name:string ->
  ?wm_class:string ->
  ?border_width:int ->
  ?width:int -> ?height:int -> ?show:bool -> unit -> assistant
