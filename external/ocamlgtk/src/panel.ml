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

type panel_applet = [`panelapplet| Gtk.event_box]

type background_type = [
  | `NO_BACKGROUND
  | `COLOR_BACKGROUND of Gdk.color
  | `PIXMAP_BACKGROUND of Gdk.pixmap
]
type flags = [
  | `EXPAND_MAJOR
  | `EXPAND_MINOR
  | `HAS_HANDLE
]
type orient_type = GtkEnums.arrow_type

external get_orient : [> panel_applet] Gtk.obj -> orient_type = "ml_panel_applet_get_orient"
external get_size : [> panel_applet] Gtk.obj -> int = "ml_panel_applet_get_size"
external get_background : [> panel_applet] Gtk.obj -> background_type = "ml_panel_applet_get_background"
external get_flags : [> panel_applet] Gtk.obj -> flags list = "ml_panel_applet_get_flags"
external set_flags : [> panel_applet] Gtk.obj -> flags list -> unit = "ml_panel_applet_set_flags"

type verb = string * (string -> unit)
external setup_menu : [> panel_applet] Gtk.obj -> xml:string -> verb list -> unit = "ml_panel_applet_setup_menu"
external setup_menu_from_file : [> panel_applet] Gtk.obj -> ?dir:string -> file:string -> ?app_name:string -> verb list -> unit = "ml_panel_applet_setup_menu_from_file"

external _factory_main : string array -> iid:string -> ([> panel_applet] Gtk.obj -> iid:string -> bool) -> bool = "ml_panel_applet_factory_main"

module S = 
  struct
  let change_background =
    { GtkSignal.name       = "change-background" ;
      GtkSignal.classe     = `panelapplet ;
      GtkSignal.marshaller =
      fun (cb : (background_type -> unit)) argv ->
	match Gobject.Closure.get_args argv with
	| _ :: `INT 0 :: _ ->
	    cb `NO_BACKGROUND
	| _ :: `INT 1 :: `POINTER (Some color) :: _ ->
	    cb (`COLOR_BACKGROUND (Obj.magic color))
	| _ :: `INT 2 :: _ :: `OBJECT (Some pixmap) :: _ ->
	    cb (`PIXMAP_BACKGROUND (Gdk.Pixmap.cast pixmap))
	| _ ->
	    failwith "marshal: PanelApplet::change-background" }
  let change_orient : (panel_applet, _) GtkSignal.t =
    { GtkSignal.name       = "change-orient" ;      
      GtkSignal.classe     = `panelapplet ;
      GtkSignal.marshaller = 
        GtkSignal.marshal1 GtkEnums.arrow_type_conv "change-orient"
    }
  let change_size : (panel_applet, _) GtkSignal.t =
    { GtkSignal.name       = "change-size" ;
      GtkSignal.classe     = `panelapplet ;
      GtkSignal.marshaller =
        GtkSignal.marshal1 Gobject.Data.int "change-size" 
    }
  let move_focus_out_of_applet : (panel_applet, _) GtkSignal.t =
    { GtkSignal.name       = "move-focus-out-of-applet" ;
      GtkSignal.classe     = `panelapplet ;
      GtkSignal.marshaller =
        GtkSignal.marshal1 GtkEnums.direction_type_conv "move-focus-out-of-applet"
    }
  end



class applet_signals obj = object (self)
  inherit GContainer.container_signals_impl obj
  method change_background        = self#connect S.change_background
  method change_orient            = self#connect S.change_orient
  method change_size              = self#connect S.change_size
  method move_focus_out_of_applet = self#connect S.move_focus_out_of_applet
end

class applet obj = object (self)
  inherit GContainer.bin obj
  method connect = new applet_signals (obj :> panel_applet Gtk.obj)
  method event   = new GObj.event_ops obj

  method get_background = get_background obj
  method get_orient     = get_orient obj
  method get_size       = get_size obj
  method get_flags      = get_flags obj
  method set_flags      = set_flags obj

  method setup_menu           = setup_menu obj
  method setup_menu_from_file = setup_menu_from_file obj
end

let factory_main ~iid cb = 
  _factory_main Sys.argv ~iid (fun obj ~iid -> cb (new applet obj) ~iid)
