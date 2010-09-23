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

(* $Id: gButton.ml 1499 2010-04-08 08:00:42Z garrigue $ *)

open Gaux
open Gobject
open Gtk
open GtkBase
open GtkButton
open OgtkButtonProps
open GObj
open GContainer

class button_skel obj = object (self)
  inherit bin obj
  inherit button_props
  method clicked () = Button.clicked obj
  method grab_default () =
    set Widget.P.can_default obj true;
    set Widget.P.has_default obj true
  method event = new GObj.event_ops obj

  method unset_image () =
    Gobject.Property.set_dyn obj 
      GtkButtonProps.Button.P.image.Gobject.name
      (`OBJECT None)
end

class button_signals obj = object
  inherit container_signals_impl (obj : [> button] obj)
  inherit button_sigs
end

class button obj = object
  inherit button_skel (obj : Gtk.button obj)
  method connect = new button_signals obj
end

let pack_return create p ?packing ?show () =
  pack_return (create p) ~packing ~show

let button ?label =
  Button.make_params [] ?label ~cont:(
  pack_return (fun p -> new button (Button.create p)))

class toggle_button_signals obj = object (self)
  inherit button_signals obj
  method toggled = self#connect ToggleButton.S.toggled
end

class toggle_button obj = object
  inherit button_skel obj
  method connect = new toggle_button_signals obj
  method active = get ToggleButton.P.active obj
  method set_active = set ToggleButton.P.active obj
  method set_draw_indicator = set ToggleButton.P.draw_indicator obj
end

let make_toggle_button create ?label =
  Button.make_params [] ?label ~cont:(
  ToggleButton.make_params ~cont:(
  pack_return (fun p -> new toggle_button (create p))))

let toggle_button = make_toggle_button ToggleButton.create
let check_button = make_toggle_button ToggleButton.create_check

class radio_button obj = object
  inherit toggle_button (obj : Gtk.radio_button obj)
  method set_group = set RadioButton.P.group obj
  method group = Some obj
end

let radio_button ?group =
  Button.make_params [] ~cont:(
  ToggleButton.make_params ~cont:(
  pack_return (fun p -> new radio_button (RadioButton.create ?group p))))

class color_button_signals obj = object (self)
  inherit button_signals obj
  method color_set = self#connect ColorButton.S.color_set
end

class color_button obj = object
  inherit button_skel obj
  inherit color_button_props
  method connect = new color_button_signals obj
end

let color_button =
  ColorButton.make_params [] ~cont:(
  pack_return (fun pl -> new color_button (ColorButton.create pl)))

class font_button_signals obj = object (self)
  inherit button_signals obj
  method font_set = self#connect FontButton.S.font_set
end

class font_button obj = object
  inherit button_skel obj
  inherit font_button_props
  method connect = new font_button_signals obj
end

let font_button =
  FontButton.make_params [] ~cont:(
  pack_return (fun pl -> new font_button (FontButton.create pl)))

(* Toolbar *)

class type tool_item_o = object
  method as_tool_item : Gtk.tool_item obj
end

class toolbar_signals obj = object
  inherit GContainer.container_signals_impl obj
  inherit toolbar_sigs
end

class toolbar obj = object
  inherit container (obj : Gtk.toolbar obj)
  method connect = new toolbar_signals obj
  method insert_widget ?tooltip ?tooltip_private ?pos w =
    Toolbar.insert_widget obj (as_widget w) ?tooltip ?tooltip_private ?pos

  method insert_button ?text ?tooltip ?tooltip_private ?icon
      ?pos ?callback () =
    let icon = may_map icon ~f:as_widget in
    new button
      (Toolbar.insert_button obj ~kind:`BUTTON ?icon ?text
	 ?tooltip ?tooltip_private ?pos ?callback ())

  method insert_toggle_button ?text ?tooltip ?tooltip_private ?icon
      ?pos ?callback () =
    let icon = may_map icon ~f:as_widget in
    new toggle_button
      (ToggleButton.cast
	 (Toolbar.insert_button obj ~kind:`TOGGLEBUTTON ?icon ?text
	    ?tooltip ?tooltip_private ?pos ?callback ()))

  method insert_radio_button ?text ?tooltip ?tooltip_private ?icon
      ?pos ?callback () =
    let icon = may_map icon ~f:as_widget in
    new radio_button
      (RadioButton.cast
	 (Toolbar.insert_button obj ~kind:`RADIOBUTTON ?icon ?text
	    ?tooltip ?tooltip_private ?pos ?callback ()))

  method insert_space = Toolbar.insert_space obj

  method orientation = get Toolbar.P.orientation obj
  method set_orientation = set Toolbar.P.orientation obj
  method style = get Toolbar.P.toolbar_style obj
  method set_style = set Toolbar.P.toolbar_style obj
  method unset_style () = Toolbar.unset_style obj
  method get_tooltips = Toolbar.get_tooltips obj
  method set_tooltips = Toolbar.set_tooltips obj
  method icon_size = Toolbar.get_icon_size obj
  method set_icon_size = Toolbar.set_icon_size obj
  method unset_icon_size () = Toolbar.unset_icon_size obj

  (* extended API in GTK 2.4 *)      
  method show_arrow = get Toolbar.P.show_arrow obj
  method set_show_arrow = set Toolbar.P.show_arrow obj
  method insert : 'a. ?pos:int -> (#tool_item_o as 'a) -> unit =
    fun ?(pos= -1) i -> Toolbar.insert obj i#as_tool_item ~pos
  method get_item_index : 'a. (#tool_item_o as 'a) -> int =
    fun i -> Toolbar.get_item_index obj i#as_tool_item
  method get_n_items = Toolbar.get_n_items obj
  method get_nth_item = Toolbar.get_nth_item obj
  method get_drop_index = Toolbar.get_drop_index obj
  method set_drop_highlight_item : 'a. ((#tool_item_o as 'a) * int) option -> unit = 
    function 
      | None -> Toolbar.set_drop_highlight_item obj None 0
      | Some (i, pos) -> Toolbar.set_drop_highlight_item obj (Some i#as_tool_item) pos
  method relief_style = Toolbar.get_relief_style obj
end

let toolbar ?orientation ?style ?tooltips =
  pack_container [] ~create:(fun p ->
    let w = Toolbar.create p in
    Toolbar.set w ?orientation ?style ?tooltips;
    new toolbar w)


(* New extended API in GTK 2.4 *)
let may_cons = Gobject.Property.may_cons

class tool_item_skel obj = object
  inherit [[> Gtk.tool_item]] GContainer.bin_impl obj
  inherit OgtkButtonProps.tool_item_props
  method as_tool_item = (obj :> Gtk.tool_item obj)
  method set_homogeneous = ToolItem.set_homogeneous obj
  method get_homogeneous = ToolItem.get_homogeneous obj
  method set_expand = ToolItem.set_expand obj
  method get_expand = ToolItem.get_expand obj
  method set_tooltip (t : GData.tooltips) =
    ToolItem.set_tooltip obj t#as_tooltips
  method set_use_drag_window = ToolItem.set_use_drag_window obj
  method get_use_drag_window = ToolItem.get_use_drag_window obj
end

class tool_item obj = object
  inherit tool_item_skel obj
  method connect = new GContainer.container_signals_impl obj
end

let tool_item_params create pl ?homogeneous ?expand ?packing ?show () = 
  let item = create pl in
  Gaux.may item#set_homogeneous homogeneous ;
  Gaux.may item#set_expand expand ;
  Gaux.may (fun f -> (f (item :> tool_item_o) : unit)) packing ;
  if show <> Some false then item#misc#show () ;
  item

let tool_item =
  tool_item_params 
    (fun pl -> new tool_item (ToolItem.create pl))
    []

class separator_tool_item obj = object
  inherit tool_item obj
  method draw = get SeparatorToolItem.P.draw obj
  method set_draw = set SeparatorToolItem.P.draw obj
end

let separator_tool_item ?draw =
  let pl = may_cons SeparatorToolItem.P.draw draw [] in
  tool_item_params 
    (fun pl -> new separator_tool_item (SeparatorToolItem.create pl))
    pl

class tool_button_signals (obj : [> Gtk.tool_button] obj) = object (self)
  inherit GContainer.container_signals_impl obj
  method clicked = self#connect ToolButton.S.clicked
end

class tool_button_skel obj = object
  inherit tool_item_skel obj
  inherit tool_button_props
end

class tool_button obj = object
  inherit tool_button_skel obj
  method connect = new tool_button_signals obj
end

let tool_button_params create pl ?label ?stock ?use_underline =
  tool_item_params create
    (may_cons ToolButton.P.label label (
     may_cons ToolButton.P.stock_id stock (
     may_cons ToolButton.P.use_underline use_underline pl)))

let tool_button =
  tool_button_params 
    (fun pl -> new tool_button (ToolButton.create pl))
    []

class toggle_tool_button_signals obj = object (self)
  inherit tool_button_signals obj
  method toggled = self#connect ToggleToolButton.S.toggled
end

class toggle_tool_button obj = object
  inherit tool_button_skel obj
  method connect = new toggle_tool_button_signals obj
  method set_active = ToggleToolButton.set_active obj
  method get_active = ToggleToolButton.get_active obj
end

let toggle_tool_button_params create pl ?active =
  tool_button_params
    (fun pl -> 
      let o = create pl in
      Gaux.may o#set_active active ;
      o)
    pl

let toggle_tool_button =
  toggle_tool_button_params 
    (fun pl -> new toggle_tool_button (ToggleToolButton.create pl))
    []

class radio_tool_button obj = object
  inherit toggle_tool_button obj
  method group = Some (obj :> Gtk.radio_tool_button Gtk.obj)
  method set_group = set RadioToolButton.P.group obj
end

let radio_tool_button ?group =
  toggle_tool_button_params
    (fun pl -> new radio_tool_button (RadioToolButton.create pl))
    (may_cons RadioToolButton.P.group 
       (Gaux.may_map (fun g -> g#group) group)
       [])

class menu_tool_button obj = object
  inherit tool_button obj
  method menu = get MenuToolButton.P.menu obj
  method set_menu = set MenuToolButton.P.menu obj
  method set_arrow_tooltip (t : GData.tooltips) =
    MenuToolButton.set_arrow_tooltip obj t#as_tooltips
end

let menu_tool_button ?menu =
  tool_button_params 
    (fun pl -> new menu_tool_button (MenuToolButton.create pl))
    (may_cons MenuToolButton.P.menu
       (Gaux.may_map (fun m -> m#as_menu) menu)
       [])
    
class link_button obj = object
  inherit button_skel obj
  inherit link_button_props
end

let link_button ?label =
  pack_return 
    (fun uri -> new link_button 
       (match label with 
        | None -> LinkButton.create uri
        | Some s -> LinkButton.create_with_label uri s))


