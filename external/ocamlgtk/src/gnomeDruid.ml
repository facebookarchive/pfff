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

type druid = [Gtk.container|`druid]
type druidpage = [Gtk.bin|`druidpage]
type page_edge = [druidpage|`page_edge]
type page_standard = [druidpage|`page_standard]

module Druid =
  struct
external new_druid : unit -> druid Gtk.obj = "ml_gnome_druid_new"
external set_buttons_sensitive : druid Gtk.obj -> back:bool -> next:bool -> cancel:bool -> help:bool -> unit = "ml_gnome_druid_set_buttons_sensitive"
external prepend_page : druid Gtk.obj -> druidpage Gtk.obj -> unit = "ml_gnome_druid_prepend_page"
external insert_page : druid Gtk.obj -> druidpage Gtk.obj -> druidpage Gtk.obj -> unit = "ml_gnome_druid_insert_page"
external append_page : druid Gtk.obj -> druidpage Gtk.obj -> unit = "ml_gnome_druid_append_page"
external set_page : druid Gtk.obj -> druidpage Gtk.obj -> unit = "ml_gnome_druid_set_page"

module Signals = struct
  let cancel =
    { GtkSignal.name       = "cancel" ;
      GtkSignal.classe     = `druid ;
      GtkSignal.marshaller = GtkSignal.marshal_unit }
  let help =
    { GtkSignal.name       = "help" ;
      GtkSignal.classe     = `druid ;
      GtkSignal.marshaller = GtkSignal.marshal_unit }
end

module Prop = struct
  let show_finish : (druid, bool) Gobject.property = 
    { Gobject.name = "show-finish" ;
      Gobject.conv = Gobject.Data.boolean }
  let show_help : (druid, bool) Gobject.property = 
    { Gobject.name = "show-help" ;
      Gobject.conv = Gobject.Data.boolean }
end

end

module Druid_page = struct
  module Signals = struct
    let back : (druidpage, druid Gtk.obj -> bool) GtkSignal.t = 
      { GtkSignal.name       = "back" ;
	GtkSignal.classe     = `druidpage ;
	GtkSignal.marshaller = 
	GtkSignal.marshal1_ret ~ret:Gobject.Data.boolean Gobject.Data.gobject "GnomeDruidPage::back"
      }
    let cancel : (druidpage, druid Gtk.obj -> bool) GtkSignal.t = 
      { GtkSignal.name       = "cancel" ;
	GtkSignal.classe     = `druidpage ;
	GtkSignal.marshaller = 
	GtkSignal.marshal1_ret ~ret:Gobject.Data.boolean Gobject.Data.gobject "GnomeDruidPage::cancel"
      }
    let finish : (druidpage, druid Gtk.obj -> unit) GtkSignal.t = 
      { GtkSignal.name       = "finish" ;
	GtkSignal.classe     = `druidpage ;
	GtkSignal.marshaller = GtkSignal.marshal1 Gobject.Data.gobject "GnomeDruidPage::finish"
      }
    let next : (druidpage, druid Gtk.obj -> bool) GtkSignal.t =
      { GtkSignal.name       = "next" ;
	GtkSignal.classe     = `druidpage ;
	GtkSignal.marshaller = 
	GtkSignal.marshal1_ret ~ret:Gobject.Data.boolean Gobject.Data.gobject "GnomeDruidPage::next"
      }
    let prepare : (druidpage, druid Gtk.obj -> unit) GtkSignal.t = 
      { GtkSignal.name       = "prepare" ;
	GtkSignal.classe     = `druidpage ;
	GtkSignal.marshaller = GtkSignal.marshal1 Gobject.Data.gobject "GnomeDruidPage::prepare"
      }
  end
end


module Page_Edge = struct
  type edge_position = [ `START | `FINISH | `OTHER ]
  external new_with_vals : edge_position -> aa:bool -> ?title:string -> ?text:string ->
    ?logo:GdkPixbuf.pixbuf -> ?watermark:GdkPixbuf.pixbuf -> ?top_watermark:GdkPixbuf.pixbuf -> 
    page_edge Gtk.obj
	= "ml_gnome_druid_page_edge_new_with_vals_bc" "ml_gnome_druid_page_edge_new_with_vals"

  external set_bg_color : page_edge Gtk.obj -> Gdk.color -> unit
      = "ml_gnome_druid_page_edge_set_bg_color"
  external set_textbox_color : page_edge Gtk.obj -> Gdk.color -> unit
      = "ml_gnome_druid_page_edge_set_textbox_color"
  external set_logo_bg_color : page_edge Gtk.obj -> Gdk.color -> unit
      = "ml_gnome_druid_page_edge_set_logo_bg_color"
  external set_title_color : page_edge Gtk.obj -> Gdk.color -> unit
      = "ml_gnome_druid_page_edge_set_title_color"
  external set_text_color : page_edge Gtk.obj -> Gdk.color -> unit
      = "ml_gnome_druid_page_edge_set_text_color"
  external set_text : page_edge Gtk.obj -> string -> unit
      = "ml_gnome_druid_page_edge_set_text"
  external set_title : page_edge Gtk.obj -> string -> unit
      = "ml_gnome_druid_page_edge_set_title"
  external set_logo : page_edge Gtk.obj -> GdkPixbuf.pixbuf -> unit
      = "ml_gnome_druid_page_edge_set_logo"
  external set_watermark : page_edge Gtk.obj -> GdkPixbuf.pixbuf -> unit
      = "ml_gnome_druid_page_edge_set_watermark"
  external set_top_watermark : page_edge Gtk.obj -> GdkPixbuf.pixbuf -> unit
      = "ml_gnome_druid_page_edge_set_top_watermark"
end

module Page_Standard = struct
  external vbox : page_standard Gtk.obj -> Gtk.box Gtk.obj = "ml_gnome_druid_page_standard_vbox"
  external new_page_standard : unit -> page_standard Gtk.obj = "ml_gnome_druid_page_standard_new"
  external append_item : page_standard Gtk.obj -> ?question:string -> Gtk.widget Gtk.obj -> ?additional_info:string -> unit = "ml_gnome_druid_page_standard_append_item"

  module Prop = struct
    let background : (page_standard, string) Gobject.property = 
      { Gobject.name = "background" ;
	Gobject.conv = Gobject.Data.string  }
    let logo : (page_standard, GdkPixbuf.pixbuf) Gobject.property =
      { Gobject.name = "logo" ;
	Gobject.conv = Gobject.Data.gobject }
    let logo_background : (page_standard, string) Gobject.property = 
      { Gobject.name = "logo-background" ;
	Gobject.conv = Gobject.Data.string }
    let title : (page_standard, string) Gobject.property = 
      { Gobject.name = "title" ;
	Gobject.conv = Gobject.Data.string }
    let title_foreground : (page_standard, string) Gobject.property = 
      { Gobject.name = "title-foreground" ;
	Gobject.conv = Gobject.Data.string }
  end
end
