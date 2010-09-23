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

(* $Id: gMenu.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gtk
open GObj
open GContainer

(** Menus *)

(** @gtkdoc gtk GtkMenuShell *)
class menu_shell_signals : [> menu_shell] obj ->
  object
    inherit GContainer.container_signals
    method deactivate : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkMenuItem *)
class menu_item_signals : [> menu_item] obj ->
  object
    inherit GContainer.item_signals
    method activate : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkMenuItem *)
class menu_item_skel :
  'a obj ->
  object
    inherit GContainer.container
    constraint 'a = [> Gtk.menu_item]
    val obj : 'a obj
    method activate : unit -> unit
    method add_accelerator :
      group:accel_group ->
      ?modi:Gdk.Tags.modifier list ->
      ?flags:Tags.accel_flag list -> Gdk.keysym -> unit
    method as_item : Gtk.menu_item obj
    method remove_submenu : unit -> unit
    method set_right_justified : bool -> unit
    method select : unit -> unit
    method deselect : unit -> unit
    method right_justified : bool
    method set_submenu : menu -> unit
    method get_submenu : GObj.widget option

  end

(** The widget used for item in menus
   @gtkdoc gtk GtkMenuItem *)
and menu_item : 'a obj ->
  object
    inherit menu_item_skel
    constraint 'a = [> Gtk.menu_item]
    val obj : 'a obj
    method event : event_ops
    method connect : menu_item_signals
  end

(** A drop down menu widget
   @gtkdoc gtk GtkMenu *)
and menu : Gtk.menu obj ->
  object
    inherit [menu_item] GContainer.item_container
    val obj : Gtk.menu obj
    method add : menu_item -> unit
    method event : event_ops
    method append : menu_item -> unit
    method as_menu : Gtk.menu obj
    method children : menu_item list
    method connect : menu_shell_signals
    method deactivate : unit -> unit
    method insert : menu_item -> pos:int -> unit
    method popdown : unit -> unit
    method popup : button:int -> time:int32 -> unit
    method prepend : menu_item -> unit
    method remove : menu_item -> unit
    method set_accel_group : accel_group -> unit
    method set_accel_path : string -> unit
    method set_border_width : int -> unit
    method private wrap : Gtk.widget obj -> menu_item
  end

(** @gtkdoc gtk GtkMenu *)
val menu :
  ?accel_path:string ->
  ?border_width:int -> ?packing:(menu -> unit) -> ?show:bool -> unit -> menu

(** @gtkdoc gtk GtkMenuItem *)
val menu_item :
  ?use_mnemonic:bool ->
  ?label:string ->
  ?right_justified:bool ->
  ?packing:(menu_item -> unit) -> ?show:bool -> unit -> menu_item

(** @gtkdoc gtk GtkTearoffMenuItem *)
val tearoff_item :
  ?packing:(menu_item -> unit) -> ?show:bool -> unit -> menu_item

(** @gtkdoc gtk GtkSeparatorMenuItem *)
val separator_item :
  ?packing:(menu_item -> unit) -> ?show:bool -> unit -> menu_item

(** A menu item with an icon
   @gtkdoc gtk GtkImageMenuItem *)
class image_menu_item : 'a obj ->
object
  inherit menu_item_skel
  constraint 'a =  Gtk.image_menu_item
  val obj : 'a obj
  method event : event_ops
  method connect : menu_item_signals
  method image : widget
  method set_image : widget -> unit
end

(** @gtkdoc gtk GtkImageMenuItem *)
val image_menu_item :
  ?image:#widget ->
  ?label:string ->
  ?use_mnemonic:bool ->
  ?stock:GtkStock.id ->
  ?right_justified:bool ->
  ?packing:(menu_item -> unit) -> ?show:bool -> unit -> image_menu_item

(** @gtkdoc gtk GtkCheckMenuItem *)
class check_menu_item_signals : [> check_menu_item] obj ->
  object
    inherit menu_item_signals
    method toggled : callback:(unit -> unit) -> GtkSignal.id
  end

(** A menu item with a check box
   @gtkdoc gtk GtkCheckMenuItem *)
class check_menu_item : 'a obj ->
  object
    inherit menu_item_skel
    constraint 'a = [> Gtk.check_menu_item]
    val obj : 'a obj
    method active : bool
    method event : event_ops
    method connect : check_menu_item_signals
    method set_active : bool -> unit
    method set_inconsistent : bool -> unit
    method inconsistent : bool
    method set_show_toggle : bool -> unit
    method toggled : unit -> unit
  end

(** @gtkdoc gtk GtkCheckMenuItem *)
val check_menu_item :
  ?label:string ->
  ?use_mnemonic:bool ->
  ?active:bool ->
  ?show_toggle:bool ->
  ?right_justified:bool ->
  ?packing:(menu_item -> unit) -> ?show:bool -> unit -> check_menu_item

(** A choice from multiple check menu items
   @gtkdoc gtk GtkRadioMenuItem *)
class radio_menu_item : Gtk.radio_menu_item obj ->
  object
    inherit check_menu_item
    val obj : Gtk.radio_menu_item obj
    method group : Gtk.radio_menu_item group
    method set_group : Gtk.radio_menu_item group -> unit
  end

(** @gtkdoc gtk GtkRadioMenuItem *)
val radio_menu_item :
  ?group:Gtk.radio_menu_item group ->
  ?label:string ->
  ?use_mnemonic:bool ->
  ?active:bool ->
  ?show_toggle:bool ->
  ?right_justified:bool ->
  ?packing:(menu_item -> unit) -> ?show:bool -> unit -> radio_menu_item

(** @gtkdoc gtk GtkMenuShell *)
class menu_shell : 'a obj ->
  object
    inherit [menu_item] GContainer.item_container
    constraint 'a = [> Gtk.menu_shell]
    val obj : 'a obj
    method event : event_ops
    method deactivate : unit -> unit
    method connect : menu_shell_signals
    method insert : menu_item -> pos:int -> unit
    method private wrap : Gtk.widget obj -> menu_item
  end

(** @gtkdoc gtk GtkMenuBar *)
val menu_bar :
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> menu_shell

(** A widget used to choose from a list of valid choices
   @gtkdoc gtk GtkOptionMenu *)
class option_menu : 'a obj ->
  object
    inherit GButton.button_skel
    constraint 'a = [> Gtk.option_menu]
    val obj : 'a obj
    method event : event_ops
    method connect : GButton.button_signals
    method get_menu : menu
    method remove_menu : unit -> unit
    method set_history : int -> unit
    method set_menu : menu -> unit
  end

(** @gtkdoc gtk GtkOptionMenu *)
val option_menu :
  ?menu:#menu ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> option_menu


(** A factory for menus
   @gtkdoc gtk GtkItemFactory *)
class ['a] factory :
  ?accel_group:accel_group ->
  ?accel_path:string -> 
  ?accel_modi:Gdk.Tags.modifier list ->
  ?accel_flags:Tags.accel_flag list ->
  'a ->
  object
    constraint 'a = #menu_shell
    val flags : Tags.accel_flag list
    val group : accel_group
    val m : Gdk.Tags.modifier list
    val menu_shell : 'a
    method accel_group : accel_group
    method add_check_item :
      ?active:bool ->
      ?key:Gdk.keysym ->
      ?callback:(bool -> unit) -> string -> check_menu_item
    method add_item :
      ?key:Gdk.keysym ->
      ?callback:(unit -> unit) ->
      ?submenu:menu -> string -> menu_item
    method add_image_item :
      ?image:widget ->
      ?key:Gdk.keysym ->
      ?callback:(unit -> unit) ->
      ?stock:GtkStock.id -> ?label:string -> unit -> image_menu_item
    method add_radio_item :
      ?group:Gtk.radio_menu_item group ->
      ?active:bool ->
      ?key:Gdk.keysym ->
      ?callback:(bool -> unit) -> string -> radio_menu_item
    method add_separator : unit -> menu_item
    method add_submenu : ?key:Gdk.keysym -> string -> menu
    method add_tearoff : unit -> menu_item
    method private bind :
      ?modi:Gdk.Tags.modifier list -> 
      ?key:Gdk.keysym -> 
      ?callback:(unit -> unit) -> menu_item -> string -> unit
    method menu : 'a
  end
