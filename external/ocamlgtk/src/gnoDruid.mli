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

(** Druids (a.k.a. Wizards) *)

class type druid_page =
  object
    method as_druidpage : GnomeDruid.druidpage Gtk.obj
  end

(** @gtkdoc libgnomeui GnomeDruid *)
class druid_signals :
  GnomeDruid.druid Gtk.obj ->
  object ('a)
    inherit GContainer.container_signals
    method cancel : callback:(unit -> unit) -> GtkSignal.id
    method help : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc libgnomeui GnomeDruid *)
class druid :
  GnomeDruid.druid Gtk.obj ->
  object
    inherit GContainer.container
    val obj : GnomeDruid.druid Gtk.obj

    method connect : druid_signals

    method set_show_finish : bool -> unit
    method set_show_help : bool -> unit
    method show_finish : bool
    method show_help : bool
    method set_buttons_sensitive :
      back:bool -> next:bool -> cancel:bool -> help:bool -> unit

    method prepend_page : #druid_page -> unit
    method insert_page : #druid_page -> #druid_page -> unit
    method append_page : #druid_page -> unit
    method set_page : #druid_page -> unit
  end

(** @gtkdoc libgnomeui GnomeDruid *)
val druid :
  ?show_finish:bool ->
  ?show_help:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> druid

(** @gtkdoc libgnomeui GnomeDruidPage *)
class druid_page_signals :
  GnomeDruid.druidpage Gtk.obj ->
  object ('a)
    inherit GContainer.container_signals
    method back : callback:(druid -> bool) -> GtkSignal.id
    method cancel : callback:(druid -> bool) -> GtkSignal.id
    method finish : callback:(druid -> unit) -> GtkSignal.id
    method next : callback:(druid -> bool) -> GtkSignal.id
    method prepare : callback:(druid -> unit) -> GtkSignal.id
  end

(** @gtkdoc libgnomeui GnomeDruidPage *)
class druid_page_skel :
  ([> GnomeDruid.druidpage] as 'a)  Gtk.obj ->
  object
    inherit GContainer.container
    val obj : 'a Gtk.obj
    method as_druidpage : GnomeDruid.druidpage Gtk.obj
    method connect : druid_page_signals
  end

(** @gtkdoc libgnomeui GnomeDruidPageEdge *)
class druid_page_edge :
  GnomeDruid.page_edge Gtk.obj ->
  object
    inherit druid_page_skel
    val obj : GnomeDruid.page_edge Gtk.obj

    method set_bg_color : Gdk.color -> unit
    method set_logo : GdkPixbuf.pixbuf -> unit
    method set_logo_bg_color : Gdk.color -> unit
    method set_resize_mode : Gtk.Tags.resize_mode -> unit
    method set_text : string -> unit
    method set_text_color : Gdk.color -> unit
    method set_textbox_color : Gdk.color -> unit
    method set_title : string -> unit
    method set_title_color : Gdk.color -> unit
    method set_top_watermark : GdkPixbuf.pixbuf -> unit
    method set_watermark : GdkPixbuf.pixbuf -> unit
  end

(** @gtkdoc libgnomeui GnomeDruidPageEdge *)
val druid_page_edge :
  position:GnomeDruid.Page_Edge.edge_position ->
  aa:bool ->
  ?title:string ->
  ?text:string ->
  ?logo:GdkPixbuf.pixbuf ->
  ?watermark:GdkPixbuf.pixbuf ->
  ?top_watermark:GdkPixbuf.pixbuf ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit ->
  druid_page_edge

(** @gtkdoc libgnomeui GnomeDruidPageStandard *)
class druid_page_standard :
  GnomeDruid.page_standard Gtk.obj ->
  object
    inherit druid_page_skel
    val obj : GnomeDruid.page_standard Gtk.obj

    method vbox : GPack.box
    method append_item :
      ?question:string -> ?additional_info:string -> GObj.widget -> unit
    method set_background : string -> unit
    method set_border_width : int -> unit
    method set_logo : GdkPixbuf.pixbuf -> unit
    method set_logo_background : string -> unit
    method set_resize_mode : Gtk.Tags.resize_mode -> unit
    method set_title : string -> unit
    method set_title_foreground : string -> unit
  end

(** @gtkdoc libgnomeui GnomeDruidPageStandard *)
val druid_page_standard :
  ?background:string ->
  ?logo:GdkPixbuf.pixbuf ->
  ?logo_background:string ->
  ?title:string ->
  ?title_foreground:string ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit ->
  druid_page_standard
