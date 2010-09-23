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

(* $Id: gObj.ml 1454 2009-05-12 10:19:38Z garrigue $ *)

open StdLabels
open Gaux
open Gobject
open Gtk
open GtkData
open GtkBase

(* GObject *)

class ['a] gobject_signals obj = object
  val obj : 'a obj = obj
  val after = false
  method after = {< after = true >}
  method private connect : 'b. ('a,'b) GtkSignal.t -> callback:'b -> _ =
    fun sgn ~callback -> GtkSignal.connect obj ~sgn ~after ~callback
end

class gobject_ops obj = object
  val obj = obj
  method get_oid = get_oid obj
  method get_type = Type.name (get_type obj)
  method disconnect = GtkSignal.disconnect obj
  method handler_block = GtkSignal.handler_block obj
  method handler_unblock = GtkSignal.handler_unblock obj
  method set_property : 'a. string -> 'a data_set -> unit =
    Property.set_dyn obj
  method get_property = Property.get_dyn obj
  method freeze_notify () = Property.freeze_notify obj
  method thaw_notify () = Property.thaw_notify obj
end

(* GtkObject *)

class type ['a] objvar = object
  val obj : 'a obj
end

class gtkobj obj = object
  val obj = obj
  method destroy () = Object.destroy obj
  method get_oid = get_oid obj
end

class gtkobj_signals_impl obj = object (self)
  inherit ['a] gobject_signals obj
  method destroy = self#connect Object.S.destroy
end

class type gtkobj_signals =
  object ('a)
    method after : 'a
    method destroy : callback:(unit -> unit) -> GtkSignal.id
  end

(* Widget *)

module Widget = GtkBase.Widget
module Event = Widget.Signals.Event
module Signals = Widget.S
module P = Widget.P

class event_signals obj = object (self)
  inherit ['a] gobject_signals (obj :> Gtk.widget obj)
  method any = self#connect Event.any
  method after_any = self#connect Signals.event_after
  method button_press = self#connect Event.button_press
  method button_release = self#connect Event.button_release
  method client = self#connect Event.client
  method configure = self#connect Event.configure
  method delete = self#connect Event.delete
  method destroy = self#connect Event.destroy
  method enter_notify = self#connect Event.enter_notify
  method expose = self#connect Event.expose
  method focus_in = self#connect Event.focus_in
  method focus_out = self#connect Event.focus_out
  method key_press = self#connect Event.key_press
  method key_release = self#connect Event.key_release
  method leave_notify = self#connect Event.leave_notify
  method map = self#connect Event.map
  method motion_notify = self#connect Event.motion_notify
  method property_notify = self#connect Event.property_notify
  method proximity_in = self#connect Event.proximity_in
  method proximity_out = self#connect Event.proximity_out
  method scroll = self#connect Event.scroll
  method selection_clear = self#connect Event.selection_clear
  method selection_notify = self#connect Event.selection_notify
  method selection_request = self#connect Event.selection_request
  method unmap = self#connect Event.unmap
  method visibility_notify = self#connect Event.visibility_notify
  method window_state = self#connect Event.window_state
end

class event_ops obj = object
  val obj = (obj :> Gtk.widget obj)
  method add = Widget.add_events obj
  method connect = new event_signals obj
  method send : Gdk.Tags.event_type Gdk.event -> bool = Widget.event obj
  method set_extensions = set Widget.P.extension_events obj
end

let iter_setcol set style =
  List.iter ~f:(fun (state, color) -> set style state (GDraw.color color))

class style st = object
  val style = st
  method as_style = style
  method copy = {< style = Style.copy style >}
  method colormap = Style.get_colormap style
  method font = Style.get_font style
  method bg = Style.get_bg style
  method set_bg = iter_setcol Style.set_bg style
  method fg = Style.get_fg style
  method set_fg = iter_setcol Style.set_fg style
  method light = Style.get_light style
  method set_light = iter_setcol Style.set_light style
  method dark = Style.get_dark style
  method set_dark = iter_setcol Style.set_dark style
  method mid = Style.get_mid style
  method set_mid = iter_setcol Style.set_mid style
  method base = Style.get_base style
  method set_base = iter_setcol Style.set_base style
  method text = Style.get_text style
  method set_text = iter_setcol Style.set_text style
  method set_font = Style.set_font style
end

class selection_input (sel : Gtk.selection_data) = object
  val sel = sel
  method selection = Selection.selection sel
  method target = Gdk.Atom.name (Selection.target sel)
end

class selection_data sel = object
  inherit selection_input sel
  method typ = Gdk.Atom.name (Selection.seltype sel)
  method data = Selection.get_data sel
  method format = Selection.format sel
end

class selection_context sel = object
  inherit selection_input sel
  method return ?typ ?(format=8) data =
    let typ =
      match typ with Some t -> Gdk.Atom.intern t | _ -> Selection.target sel in
    Selection.set sel ~typ ~format ~data:(Some data)
end

class drag_signals obj = object (self)
  inherit ['a] gobject_signals obj
  method private connect_drag : 'b. ('a, Gdk.drag_context -> 'b) GtkSignal.t ->
    callback:(drag_context -> 'b) -> _ =
      fun sgn ~callback ->
        self#connect sgn (fun context -> callback (new drag_context context))
  method beginning = self#connect_drag Signals.drag_begin
  method ending = self#connect_drag Signals.drag_end
  method data_delete = self#connect_drag Signals.drag_data_delete
  method leave = self#connect_drag Signals.drag_leave
  method motion = self#connect_drag Signals.drag_motion
  method drop = self#connect_drag Signals.drag_drop
  method data_get ~callback =
    self#connect Signals.drag_data_get ~callback:
      begin fun context seldata ~info ~time ->
        callback (new drag_context context) (new selection_context seldata)
          ~info ~time
      end
  method data_received ~callback =
    self#connect Signals.drag_data_received
      ~callback:(fun context ~x ~y data -> callback (new drag_context context)
	       ~x ~y (new selection_data data))

end

and drag_ops obj = object
  val obj = obj
  method connect = new drag_signals obj
  method dest_set ?(flags=[`ALL]) ?(actions=[]) targets =
    DnD.dest_set obj ~flags ~actions ~targets:(Array.of_list targets)
  method dest_unset () = DnD.dest_unset obj
  method get_data ~target ?(time=Int32.zero) (context : drag_context) =
    DnD.get_data obj context#context ~target:(Gdk.Atom.intern target) ~time
  method highlight () = DnD.highlight obj
  method unhighlight () = DnD.unhighlight obj
  method source_set ?modi:m ?(actions=[]) targets =
    DnD.source_set obj ?modi:m ~actions ~targets:(Array.of_list targets)
  method source_set_icon ?(colormap = Gdk.Color.get_system_colormap ())
      (pix : GDraw.pixmap) =
    DnD.source_set_icon obj ~colormap pix#pixmap ?mask:pix#mask
  method source_unset () = DnD.source_unset obj
end

and drag_context context = object
  inherit GDraw.drag_context context
  method context = context
  method finish = DnD.finish context
  method source_widget =
    new widget (unsafe_cast (DnD.get_source_widget context))
  method set_icon_widget (w : widget) =
    DnD.set_icon_widget context (w#as_widget)
  method set_icon_pixmap ?(colormap = Gdk.Color.get_system_colormap ())
      (pix : GDraw.pixmap) =
    DnD.set_icon_pixmap context ~colormap pix#pixmap ?mask:pix#mask
end

and misc_signals obj = object (self)
  inherit gtkobj_signals_impl obj
  method show = self#connect Signals.show
  method hide = self#connect Signals.hide
  method map = self#connect Signals.map
  method unmap = self#connect Signals.unmap
  method query_tooltip = self#connect Signals.query_tooltip
  method realize = self#connect Signals.realize
  method unrealize = self#connect Signals.unrealize
  method state_changed = self#connect Signals.state_changed
  method size_allocate = self#connect Signals.size_allocate
  method parent_set ~callback =
    self#connect Signals.parent_set ~callback:
      begin function
	  None   -> callback None
	| Some w -> callback (Some (new widget (unsafe_cast w)))
      end
  method style_set ~callback =
    self#connect Signals.style_set ~callback:
      (fun opt -> callback (may opt ~f:(new style)))
  method selection_get ~callback =
    self#connect Signals.selection_get ~callback:
      begin fun seldata ~info ~time ->
        callback (new selection_context seldata) ~info ~time
      end
  method selection_received ~callback =
    self#connect Signals.selection_received
      ~callback:(fun data -> callback (new selection_data data)) 
end

and misc_ops obj = object (self)
  inherit gobject_ops obj
  method get_flag = Object.get_flag obj
  method connect = new misc_signals obj
  method show () = Widget.show obj
  method unparent () = Widget.unparent obj
  method show_all () = Widget.show_all obj
  method hide () = Widget.hide obj
  method hide_all () = Widget.hide_all obj
  method map () = Widget.map obj
  method unmap () = Widget.unmap obj
  method realize () = Widget.realize obj
  method unrealize () = Widget.unrealize obj
  method draw = Widget.draw obj
  method activate () = Widget.activate obj
  method reparent (w : widget) =  Widget.reparent obj w#as_widget
  (* method popup = popup obj *)
  method intersect = Widget.intersect obj
  method grab_focus () = set P.has_focus obj true
  method grab_default () = set P.has_default obj true
  method is_ancestor (w : widget) = Widget.is_ancestor obj w#as_widget
  method add_accelerator ~sgn:sg ~group ?modi ?flags key =
    Widget.add_accelerator obj ~sgn:sg group ~key ?modi ?flags
  method remove_accelerator ~group ?modi key =
    Widget.remove_accelerator obj group ~key ?modi
  (* method lock_accelerators () = lock_accelerators obj *)
  method set_name = set P.name obj
  method set_state = Widget.set_state obj
  method set_sensitive = set P.sensitive obj
  method set_can_default = set P.can_default obj
  method set_can_focus = set P.can_focus obj
  method set_app_paintable = set P.app_paintable obj
  method set_double_buffered = Widget.set_double_buffered obj
  method set_size_request =
    Widget.size_params [] ~cont:(fun p () -> set_params obj p)
  method set_size_chars ?desc ?lang ?width ?height () =
    let metrics = 
      (self#pango_context : GPango.context)#get_metrics ?desc ?lang () in
    let width = may_map width ~f:
        (fun w -> w * GPango.to_pixels metrics#approx_digit_width)
    and height = may_map height ~f:
        (fun h -> h * GPango.to_pixels (metrics#ascent+metrics#descent)) in
    self#set_size_request ?width ?height ()
  method set_style (style : style) = set P.style obj style#as_style
  method modify_fg = iter_setcol Widget.modify_fg obj
  method modify_bg = iter_setcol Widget.modify_bg obj
  method modify_text = iter_setcol Widget.modify_text obj
  method modify_base = iter_setcol Widget.modify_base obj
  method modify_font = Widget.modify_font obj
  method modify_font_by_name s =
    Widget.modify_font obj (Pango.Font.from_string s)
  method create_pango_context =
    new GPango.context_rw (Widget.create_pango_context obj)
  (* get functions *)
  method name = get P.name obj
  method toplevel =
    try new widget (unsafe_cast (Widget.get_toplevel obj))
    with Gpointer.Null -> failwith "GObj.misc_ops#toplevel"
  method window = Widget.window obj
  method colormap = Widget.get_colormap obj
  method visual = Widget.get_visual obj
  method visual_depth = Gdk.Visual.depth (Widget.get_visual obj)
  method pointer = Widget.get_pointer obj
  method style = new style (get P.style obj)
  method visible = self#get_flag `VISIBLE
  method parent =
    may_map (fun w -> new widget (unsafe_cast w)) (get P.parent obj)
  method allocation = Widget.allocation obj
  method pango_context = new GPango.context (Widget.get_pango_context obj)
  (* icon *)
  method render_icon ?detail ~size id =
    Widget.render_icon obj (GtkStock.convert_id id) size detail
  (* selection *)
  method convert_selection ~target ?(time=Int32.zero) sel =
    Selection.convert obj ~sel ~target:(Gdk.Atom.intern target) ~time
  method grab_selection ?(time=Int32.zero) sel =
    Selection.owner_set obj ~sel ~time
  method add_selection_target ~target ?(info=0) sel =
    Selection.add_target obj ~sel ~target:(Gdk.Atom.intern target) ~info
  method clear_selection_targets sel = Selection.clear_targets obj ~sel
  (* tooltip *)
  method has_tooltip = get P.has_tooltip obj
  method tooltip_markup = get P.tooltip_markup obj
  method tooltip_text = get P.tooltip_text obj
  method set_has_tooltip = set P.has_tooltip obj
  method set_tooltip_markup = set P.tooltip_markup obj
  method set_tooltip_text = set P.tooltip_text obj
end

and widget obj = object (self)
  inherit gtkobj obj
  method as_widget = (obj :> Gtk.widget obj)
  method misc = new misc_ops (obj :> Gtk.widget obj)
  method drag = new drag_ops (unsafe_cast obj : Gtk.widget obj)
  method coerce = (self :> widget)
end

(* just to check that GDraw.misc_ops is compatible with misc_ops *)
let _ = fun (x : #GDraw.misc_ops) -> (x : misc_ops)

class widget_signals_impl (obj : [>Gtk.widget] obj) = gtkobj_signals_impl obj

class type widget_signals = gtkobj_signals

class ['a] widget_impl (obj : 'a obj) = widget obj

class widget_full obj = object
  inherit widget obj
  method connect = new widget_signals_impl obj
end

let as_widget (w : widget) = w#as_widget

let wrap_widget w = new widget (unsafe_cast w)
let unwrap_widget w = unsafe_cast w#as_widget
let conv_widget_option =
  { kind = `OBJECT;
    proj = (function `OBJECT c -> may_map ~f:wrap_widget c
           | _ -> failwith "GObj.get_object");
    inj = (fun c -> `OBJECT (may_map ~f:unwrap_widget c)) }
let conv_widget =
  { kind = `OBJECT;
    proj = (function `OBJECT (Some c) -> wrap_widget c
           | `OBJECT None -> raise Gpointer.Null
           | _ -> failwith "GObj.get_object");
    inj = (fun c -> `OBJECT (Some (unwrap_widget c))) }


let pack_return self ~packing ~show =
  may packing ~f:(fun f -> (f (self :> widget) : unit));
  if show <> Some false then self#misc#show ();
  self
