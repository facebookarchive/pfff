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

(* $Id: gButton.mli 1515 2010-06-08 08:50:23Z garrigue $ *)

open Gtk
open GObj
open GContainer

(** A widget that creates a signal when clicked on *)

(** {3 GtkButton} *)

(** @gtkdoc gtk GtkButton *)
class button_skel : 'a obj ->
  object
    inherit GContainer.bin
    constraint 'a = [> button]
    val obj : 'a obj
    method clicked : unit -> unit
    method set_relief : Tags.relief_style -> unit
    method relief : Tags.relief_style
    method set_label : string -> unit
    method label : string
    method set_use_stock : bool -> unit
    method use_stock : bool
    method set_use_underline : bool -> unit
    method use_underline : bool
    method grab_default : unit -> unit
    method event : event_ops
    method set_focus_on_click : bool -> unit
    method focus_on_click : bool
    method image : GObj.widget             (** @since GTK 2.6 *)
    method set_image : GObj.widget -> unit (** @since GTK 2.6 *)
    method unset_image : unit -> unit      (** @since GTK 2.6 *)
    method image_position : GtkEnums.position_type  (** @since GTK 2.10 *)
    method set_image_position : GtkEnums.position_type -> unit 
	(** @since GTK 2.10 *)
 
    method set_xalign : float -> unit  (** @since GTK 2.4 *)
    method xalign     : float          (** @since GTK 2.4 *)
    method set_yalign : float -> unit  (** @since GTK 2.4 *)
    method yalign     : float          (** @since GTK 2.4 *)
  end

(** @gtkdoc gtk GtkButton *)
class button_signals : 'b obj ->
  object ('a)
    inherit GContainer.container_signals
    constraint 'b = [> button]
    val obj : 'b obj
    method clicked : callback:(unit -> unit) -> GtkSignal.id
    method enter : callback:(unit -> unit) -> GtkSignal.id
    method leave : callback:(unit -> unit) -> GtkSignal.id
    method pressed : callback:(unit -> unit) -> GtkSignal.id
    method released : callback:(unit -> unit) -> GtkSignal.id
  end

(** A widget that creates a signal when clicked on
   @gtkdoc gtk GtkButton *)
class button : Gtk.button obj ->
  object
    inherit button_skel
    val obj : Gtk.button obj
    method connect : button_signals
  end

(** @gtkdoc gtk GtkButton *)
val button :
  ?label:string ->
  ?use_mnemonic:bool ->
  ?stock:GtkStock.id ->
  ?relief:Tags.relief_style ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> button

(** {4 GtkToggleButton & GtkRadioButton} *)

(** @gtkdoc gtk GtkToggleButton *)
class toggle_button_signals : 'b obj ->
  object ('a)
    inherit button_signals
    constraint 'b = [> toggle_button]
    val obj : 'b obj
    method toggled : callback:(unit -> unit) -> GtkSignal.id
  end

(** Create buttons which retain their state
   @gtkdoc gtk GtkToggleButton *)
class toggle_button :
  'a obj ->
  object
    inherit button_skel
    constraint 'a = [> Gtk.toggle_button]
    val obj : 'a obj
    method active : bool
    method connect : toggle_button_signals
    method set_active : bool -> unit
    method set_draw_indicator : bool -> unit
  end

(** @gtkdoc gtk GtkToggleButton *)
val toggle_button :
  ?label:string ->
  ?use_mnemonic:bool ->
  ?stock:GtkStock.id ->
  ?relief:Tags.relief_style ->
  ?active:bool ->
  ?draw_indicator:bool ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> toggle_button

(** @gtkdoc gtk GtkCheckButton *)
val check_button :
  ?label:string ->
  ?use_mnemonic:bool ->
  ?stock:GtkStock.id ->
  ?relief:Tags.relief_style ->
  ?active:bool ->
  ?draw_indicator:bool ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> toggle_button

(** A choice from multiple check buttons
   @gtkdoc gtk GtkRadioButton *)
class radio_button :
  Gtk.radio_button obj ->
  object
    inherit toggle_button
    val obj : Gtk.radio_button obj
    method group : Gtk.radio_button group
    method set_group : Gtk.radio_button group -> unit
  end

(** @gtkdoc gtk GtkRadioButton *)
val radio_button :
  ?group:Gtk.radio_button group ->
  ?label:string ->
  ?use_mnemonic:bool ->
  ?stock:GtkStock.id ->
  ?relief:Tags.relief_style ->
  ?active:bool ->
  ?draw_indicator:bool ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> radio_button

(** {4 GtkColorButton & GtkFontButton} *)

(** @gtkdoc gtk GtkColorButton
    @since GTK 2.4 *)
class color_button_signals :
  ([> Gtk.color_button] as 'a) Gtk.obj ->
  object
    inherit button_signals
    val obj : 'a Gtk.obj
    method color_set : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkColorButton
    @since GTK 2.4 *)
class color_button :
  ([> Gtk.color_button] as 'a) Gtk.obj ->
  object
    inherit button_skel
    val obj : 'a Gtk.obj
    method alpha : int
    method set_alpha : int -> unit
    method color : Gdk.color
    method set_color : Gdk.color -> unit
    method title : string
    method set_title : string -> unit
    method use_alpha : bool
    method set_use_alpha : bool -> unit
    method connect : color_button_signals
  end

(** A button to launch a color selection dialog
    @gtkdoc gtk GtkColorButton
    @since GTK 2.4 *)
val color_button :
  ?color:Gdk.color ->
  ?title:string ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> color_button


(** @gtkdoc gtk GtkFontButton
    @since GTK 2.4 *)
class font_button_signals :
  ([> Gtk.font_button] as 'a) Gtk.obj ->
  object
    inherit button_signals
    val obj : 'a Gtk.obj
    method font_set : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkFontButton
    @since GTK 2.4 *)
class font_button :
  ([> Gtk.font_button] as 'a) Gtk.obj ->
  object
    inherit button_skel
    val obj : 'a Gtk.obj
    method font_name : string
    method set_font_name : string -> unit
    method show_size : bool
    method set_show_size : bool -> unit
    method show_style : bool
    method set_show_style : bool -> unit
    method title : string
    method set_title : string -> unit
    method use_font : bool
    method set_use_font : bool -> unit
    method use_size : bool
    method set_use_size : bool -> unit
    method connect : font_button_signals
  end


(** A button to launch a font selection dialog
    @gtkdoc gtk GtkFontButton
    @since GTK 2.4 *)
val font_button :
  ?font_name:string ->
  ?title:string ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> font_button

(** {3 GtkToolbar} *)


class type tool_item_o = object
  method as_tool_item : Gtk.tool_item obj
end

(** @gtkdoc gtk GtkToolbar *)
class toolbar_signals :
  ([> Gtk.toolbar] as 'a) obj ->
  object
    inherit GContainer.container_signals
    method orientation_changed :
        callback:(GtkEnums.orientation -> unit) -> GtkSignal.id
    method style_changed :
        callback:(GtkEnums.toolbar_style -> unit) -> GtkSignal.id
    method focus_home_or_end : callback:(bool -> bool) -> GtkSignal.id (** @since GTK 2.4 *)
    method move_focus :
        callback:(GtkEnums.direction_type -> bool) -> GtkSignal.id (** @since GTK 2.4 *)
    method popup_context_menu :
        callback:(int -> int -> int -> bool) -> GtkSignal.id (** @since GTK 2.4 *)
  end

(** Create bars of buttons and other widgets 
   @gtkdoc gtk GtkToolbar *)
class toolbar :
  Gtk.toolbar obj ->
  object
    inherit GContainer.container
    val obj : Gtk.toolbar obj
    method connect : toolbar_signals
    method insert_button :
      ?text:string ->
      ?tooltip:string ->
      ?tooltip_private:string ->
      ?icon:widget ->
      ?pos:int -> ?callback:(unit -> unit) -> unit -> button
    method insert_radio_button :
      ?text:string ->
      ?tooltip:string ->
      ?tooltip_private:string ->
      ?icon:widget ->
      ?pos:int -> ?callback:(unit -> unit) -> unit -> radio_button
    method insert_space : ?pos:int -> unit -> unit
    method insert_toggle_button :
      ?text:string ->
      ?tooltip:string ->
      ?tooltip_private:string ->
      ?icon:widget ->
      ?pos:int -> ?callback:(unit -> unit) -> unit -> toggle_button
    method insert_widget :
      ?tooltip:string ->
      ?tooltip_private:string -> ?pos:int -> widget -> unit
    method orientation : Tags.orientation
    method set_orientation : Tags.orientation -> unit
    method style : Tags.toolbar_style
    method set_style : Tags.toolbar_style -> unit
    method unset_style : unit -> unit
    method icon_size : Tags.icon_size
    method set_icon_size : Tags.icon_size -> unit
    method unset_icon_size : unit -> unit
    method get_tooltips : bool
    method set_tooltips : bool -> unit

    (** Extended API, available in GTK 2.4 *)

    method show_arrow : bool (** @since GTK 2.4 *)
    method set_show_arrow : bool -> unit (** @since GTK 2.4 *)
    method relief_style : Tags.relief_style (** @since GTK 2.4 *)
    method get_drop_index : int -> int -> int (** @since GTK 2.4 *)
    method set_drop_highlight_item : (#tool_item_o * int) option -> unit (** @since GTK 2.4 *)
    method get_item_index : #tool_item_o -> int (** @since GTK 2.4 *)
    method get_n_items : int (** @since GTK 2.4 *)
    method get_nth_item : int -> [`toolitem ] Gtk.obj (** @since GTK 2.4 *)
    method insert : ?pos:int -> #tool_item_o -> unit 
    (** @since GTK 2.4 
        @param pos default value is [-1] i.e. end of the toolbar *)
  end

(** @gtkdoc gtk GtkToolbar *)
val toolbar :
  ?orientation:Tags.orientation ->
  ?style:Tags.toolbar_style ->
  ?tooltips:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> toolbar


(** {4 ToolItems for the new toolbar API} *)

(** @gtkdoc gtk GtkToolItem 
    @since GTK 2.4 *)
class tool_item_skel :
  ([> Gtk.tool_item] as 'a) obj ->
  object
    inherit GContainer.bin
    val obj : 'a obj
    method as_tool_item : Gtk.tool_item obj
    method is_important : bool
    method set_is_important : bool -> unit
    method visible_horizontal : bool
    method set_visible_horizontal : bool -> unit
    method visible_vertical : bool
    method set_visible_vertical : bool -> unit
    method set_homogeneous : bool -> unit
    method get_homogeneous : bool
    method set_expand : bool -> unit
    method get_expand : bool
    method set_tooltip : GData.tooltips -> string -> string -> unit
    method set_use_drag_window : bool -> unit
    method get_use_drag_window : bool
end

(** @gtkdoc gtk GtkToolItem 
    @since GTK 2.4 *)
class tool_item : 
  ([> Gtk.tool_item] as 'a) obj ->
  object
    inherit tool_item_skel
    val obj : 'a obj
    method connect : GContainer.container_signals
  end

(** @gtkdoc gtk GtkToolItem 
    @since GTK 2.4 *)
val tool_item :
  ?homogeneous:bool ->
  ?expand:bool ->
  ?packing:(tool_item_o -> unit) ->
  ?show:bool -> unit -> tool_item

(** @gtkdoc gtk GtkSeparatorToolItem
    @since GTK 2.4 *)
class separator_tool_item :
  ([> Gtk.separator_tool_item] as 'a) obj ->
  object
    inherit tool_item
    val obj : 'a obj
    method draw : bool
    method set_draw : bool -> unit
  end

(** @gtkdoc gtk GtkSeparatorToolItem
    @since GTK 2.4 *)
val separator_tool_item :
  ?draw:bool ->
  ?homogeneous:bool ->
  ?expand:bool ->
  ?packing:(tool_item_o -> unit) ->
  ?show:bool -> unit -> separator_tool_item

(** @gtkdoc gtk GtkToolButton
    @since GTK 2.4 *)
class tool_button_signals :
  ([> Gtk.tool_button] as 'a) obj ->
  object
    inherit GContainer.container_signals
    val obj : 'a obj
    method clicked : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkToolButton
    @since GTK 2.4 *)
class tool_button_skel :
  ([> Gtk.tool_button] as 'a) obj ->
  object
    inherit tool_item_skel
    val obj : 'a obj
    method icon_widget : GObj.widget
    method set_icon_widget : GObj.widget -> unit
    method label : string
    method set_label : string -> unit
    method label_widget : GObj.widget
    method set_label_widget : GObj.widget -> unit
    method stock_id : GtkStock.id
    method set_stock_id : GtkStock.id -> unit
    method use_underline : bool
    method set_use_underline : bool -> unit
  end

(** @gtkdoc gtk GtkToolButton
    @since GTK 2.4 *)
class tool_button : 
  ([> Gtk.tool_button] as 'a) obj ->
  object
    inherit tool_button_skel
    val obj : 'a obj
    method connect : tool_button_signals
  end

(** @gtkdoc gtk GtkToolButton
    @since GTK 2.4 *)
val tool_button :
  ?label:string ->
  ?stock:GtkStock.id ->
  ?use_underline:bool ->
  ?homogeneous:bool ->
  ?expand:bool ->
  ?packing:(tool_item_o -> unit) ->
  ?show:bool -> unit -> tool_button

(** @gtkdoc gtk GtkToggleToolButton
    @since GTK 2.4 *)
class toggle_tool_button_signals :
  ([> Gtk.toggle_tool_button] as 'a) obj ->
  object
    inherit tool_button_signals
    val obj : 'a obj
    method toggled : callback:(unit -> unit) -> GtkSignal.id
  end

(** @gtkdoc gtk GtkToggleToolButton
    @since GTK 2.4 *)
class toggle_tool_button :
  ([> Gtk.toggle_tool_button] as 'a) obj -> 
  object
    inherit tool_button_skel
    val obj : 'a obj
    method connect : toggle_tool_button_signals
    method set_active : bool -> unit
    method get_active : bool
  end

(** @gtkdoc gtk GtkToggleToolButton
    @since GTK 2.4 *)
val toggle_tool_button :
  ?active:bool ->
  ?label:string ->
  ?stock:GtkStock.id ->
  ?use_underline:bool ->
  ?homogeneous:bool ->
  ?expand:bool ->
  ?packing:(tool_item_o -> unit) ->
  ?show:bool -> unit -> toggle_tool_button

(** @gtkdoc gtk GtkRadioToolButton
    @since GTK 2.4 *)
class radio_tool_button :
  ([> Gtk.radio_tool_button] as 'a) obj ->
  object
    inherit toggle_tool_button
    val obj : 'a obj
    method group : Gtk.radio_tool_button group
    method set_group : Gtk.radio_tool_button group -> unit
  end

(** @gtkdoc gtk GtkRadioToolButton
    @since GTK 2.4 *)
val radio_tool_button :
  ?group:radio_tool_button ->
  ?active:bool ->
  ?label:string ->
  ?stock:GtkStock.id ->
  ?use_underline:bool ->
  ?homogeneous:bool ->
  ?expand:bool ->
  ?packing:(tool_item_o -> unit) ->
  ?show:bool -> unit -> radio_tool_button

(** @gtkdoc gtk GtkMenuToolButton
    @since GTK 2.6 *)
class menu_tool_button :
  ([> Gtk.menu_tool_button] as 'a) obj ->
  object
    inherit tool_button
    val obj : 'a obj
    method menu : Gtk.menu Gtk.obj
    method set_menu : Gtk.menu Gtk.obj -> unit
    method set_arrow_tooltip : GData.tooltips -> string -> string -> unit
  end

(** @gtkdoc gtk GtkMenuToolButton
    @since GTK 2.6 *)
val menu_tool_button :
  ?menu:<as_menu:Gtk.menu Gtk.obj;..> ->
  ?label:string ->
  ?stock:GtkStock.id ->
  ?use_underline:bool ->
  ?homogeneous:bool ->
  ?expand:bool ->
  ?packing:(tool_item_o -> unit) -> 
  ?show:bool -> unit -> menu_tool_button


(** @gtkdoc gtk GtkLinkButton
    @since GTK 2.10 *)
class link_button :
  ([> Gtk.link_button] as 'a) Gtk.obj ->
  object
    inherit button_skel
    val obj : 'a Gtk.obj
    method uri : string
    method set_uri : string -> unit
  end

(** A button for URL
    @gtkdoc gtk GtkLinkButton
    @since GTK 2.10 *)

val link_button :
  ?label:string ->
  string ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> link_button
