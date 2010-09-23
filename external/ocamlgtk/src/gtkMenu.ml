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

(* $Id: gtkMenu.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gobject
open Gtk
open GtkMenuProps
open GtkBase

external _gtkmenu_init : unit -> unit = "ml_gtkmenu_init"
let () = _gtkmenu_init ()

module MenuItem = struct
  include MenuItem
  external create_with_label : string -> menu_item obj
      = "ml_gtk_menu_item_new_with_label"
  external create_with_mnemonic : string -> menu_item obj
      = "ml_gtk_menu_item_new_with_mnemonic"
  external separator_create : unit -> menu_item obj
      = "ml_gtk_separator_menu_item_new"
  external tearoff_create : unit -> menu_item obj
      = "ml_gtk_tearoff_menu_item_new"
  let create ?(use_mnemonic=false) ?label () =
    match label with None -> create []
    | Some label -> if use_mnemonic then 
	create_with_mnemonic label else create_with_label label
end

module ImageMenuItem = struct
  include ImageMenuItem
  external create_with_label : string -> image_menu_item obj
      = "ml_gtk_image_menu_item_new_with_label"
  external create_with_mnemonic : string -> image_menu_item obj
      = "ml_gtk_image_menu_item_new_with_mnemonic"
  external create_from_stock : string -> accel_group option 
    -> image_menu_item obj = "ml_gtk_image_menu_item_new_from_stock"
  let create_from_stock ?accel_group s = 
    create_from_stock (GtkStock.convert_id s) accel_group
  let create ?label ?(use_mnemonic=false) ?stock ?accel_group () = 
    match stock with 
    | None -> 
	begin match label with
	| None -> create []
	| Some l -> 
	    if use_mnemonic then create_with_mnemonic l 
	    else create_with_label l 
	end
    | Some s -> create_from_stock ?accel_group s
end

module CheckMenuItem = struct
  include CheckMenuItem
  external create_with_label : string -> check_menu_item obj
      = "ml_gtk_check_menu_item_new_with_label"
  external create_with_mnemonic : string -> check_menu_item obj
      = "ml_gtk_check_menu_item_new_with_mnemonic"
  let create ?(use_mnemonic=false) ?label () =
    match label with None -> create []
    | Some label -> if use_mnemonic then 
	create_with_mnemonic label
      else create_with_label label
  let set ?active ?show_toggle ?right_justified w =
    may active ~f:(set P.active w);
    may show_toggle ~f:(set_show_toggle w);
    may right_justified ~f:(MenuItem.set_right_justified w)
end

module RadioMenuItem = struct
  include RadioMenuItem
  external create : radio_menu_item group -> radio_menu_item obj
      = "ml_gtk_radio_menu_item_new"
  external create_with_label :
      radio_menu_item group -> string -> radio_menu_item obj
      = "ml_gtk_radio_menu_item_new_with_label"
  external create_with_mnemonic :
      radio_menu_item group -> string -> radio_menu_item obj
      = "ml_gtk_radio_menu_item_new_with_mnemonic"
  let create ?(group = None) ?(use_mnemonic=false) ?label () =
    match label with None -> create group
    | Some label -> 
	if use_mnemonic then create_with_mnemonic group label
	else create_with_label group label
end

module OptionMenu = OptionMenu

module MenuShell = MenuShell

module Menu = struct
  include Menu
  let popup ?parent_menu ?parent_item w =
    popup w (Gpointer.optboxed parent_menu) (Gpointer.optboxed parent_item)
  let set ?active ?accel_group w =
    may active ~f:(set_active w);
    may accel_group ~f:(set_accel_group w)
end

module MenuBar = MenuBar
