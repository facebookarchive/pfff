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

(* $Id: gtkBase.ml 1454 2009-05-12 10:19:38Z garrigue $ *)

open Gaux
open Gobject
open Gtk
open Tags
open GtkBaseProps

module Object = struct
  include GtkObject
  let try_cast = Gobject.try_cast
  external destroy : [>`gtk] obj -> unit = "ml_gtk_object_destroy"
  external get_flags : [>`gtk] obj -> int = "ml_GTK_OBJECT_FLAGS"
  let get_flag obj wf =
    (get_flags obj) land (Gpointer.encode_variant GtkEnums.widget_flags wf)
      <> 0
  module S = struct
    open GtkSignal
    let destroy =
      { name = "destroy"; classe = `gtk; marshaller = marshal_unit }
  end
end

module Widget = struct
  include Widget

  let size_params ~cont pl ?width ?height =
    let may_cons = Property.may_cons in
    cont (
    may_cons P.width_request width (
    may_cons P.height_request height pl))

  external unparent : [>`widget] obj -> unit = "ml_gtk_widget_unparent"
  external show : [>`widget] obj -> unit = "ml_gtk_widget_show"
  external show_now : [>`widget] obj -> unit = "ml_gtk_widget_show_now"
  external show_all : [>`widget] obj -> unit = "ml_gtk_widget_show_all"
  external hide : [>`widget] obj -> unit = "ml_gtk_widget_hide"
  external hide_all : [>`widget] obj -> unit = "ml_gtk_widget_hide_all"
  external map : [>`widget] obj -> unit = "ml_gtk_widget_map"
  external unmap : [>`widget] obj -> unit = "ml_gtk_widget_unmap"
  external realize : [>`widget] obj -> unit = "ml_gtk_widget_realize"
  external unrealize : [>`widget] obj -> unit = "ml_gtk_widget_unrealize"
  external queue_draw : [>`widget] obj -> unit = "ml_gtk_widget_queue_draw"
  external queue_resize : [>`widget] obj -> unit = "ml_gtk_widget_queue_resize"
  external draw : [>`widget] obj -> Gdk.Rectangle.t option -> unit
      = "ml_gtk_widget_draw"
(*
  external draw_focus : [>`widget] obj -> unit
      = "ml_gtk_widget_draw_focus"
  external draw_default : [>`widget] obj -> unit
      = "ml_gtk_widget_draw_default"
*)
  external event : [>`widget] obj -> 'a Gdk.event -> bool
      = "ml_gtk_widget_event"
  external activate : [>`widget] obj -> bool
      = "ml_gtk_widget_activate"
  external reparent : [>`widget] obj -> [>`widget] obj -> unit
      = "ml_gtk_widget_reparent"
(*
  external popup : [>`widget] obj -> x:int -> y:int -> unit
      = "ml_gtk_widget_popup"
*)
  external intersect :
      [>`widget] obj -> Gdk.Rectangle.t -> Gdk.Rectangle.t option
      = "ml_gtk_widget_intersect"
  external set_state : [>`widget] obj -> state_type -> unit
      = "ml_gtk_widget_set_state"
  external set_uposition : [>`widget] obj -> x:int -> y:int -> unit
      = "ml_gtk_widget_set_uposition"
  external add_events : [>`widget] obj -> Gdk.Tags.event_mask list -> unit
      = "ml_gtk_widget_add_events"
  external get_toplevel : [>`widget] obj -> widget obj
      = "ml_gtk_widget_get_toplevel"
  external get_ancestor : [>`widget] obj -> g_type -> widget obj
      = "ml_gtk_widget_get_ancestor"
  external get_colormap : [>`widget] obj -> Gdk.colormap
      = "ml_gtk_widget_get_colormap"
  external get_visual : [>`widget] obj -> Gdk.visual
      = "ml_gtk_widget_get_visual"
  external get_pointer : [>`widget] obj -> int * int
      = "ml_gtk_widget_get_pointer"
  external is_ancestor : [>`widget] obj -> [>`widget] obj -> bool
      = "ml_gtk_widget_is_ancestor"
  external ensure_style : [>`widget] obj -> unit
      = "ml_gtk_widget_ensure_style"
  external modify_fg : [>`widget] obj -> state_type -> Gdk.color -> unit
      = "ml_gtk_widget_modify_fg"
  external modify_bg : [>`widget] obj -> state_type -> Gdk.color -> unit
      = "ml_gtk_widget_modify_bg"
  external modify_text : [>`widget] obj -> state_type -> Gdk.color -> unit
      = "ml_gtk_widget_modify_text"
  external modify_base : [>`widget] obj -> state_type -> Gdk.color -> unit
      = "ml_gtk_widget_modify_base"
  external modify_font : [>`widget] obj -> Pango.font_description -> unit
      = "ml_gtk_widget_modify_font"
  external get_pango_context : [>`widget] obj -> Pango.context
      = "ml_gtk_widget_get_pango_context"
  external create_pango_context : [>`widget] obj -> Pango.context
      = "ml_gtk_widget_create_pango_context"
  external render_icon : 
      [>`widget] obj -> string -> 
      Gtk.Tags.icon_size -> string option -> GdkPixbuf.pixbuf
      = "ml_gtk_widget_render_icon"
  external add_accelerator :
      ([>`widget] as 'a) obj -> sgn:('a,unit->unit) GtkSignal.t ->
      accel_group -> key:Gdk.keysym -> ?modi:Gdk.Tags.modifier list ->
      ?flags:accel_flag list -> unit
      = "ml_gtk_widget_add_accelerator_bc" "ml_gtk_widget_add_accelerator"
  external remove_accelerator :
      [>`widget] obj -> accel_group ->
      key:Gdk.keysym -> ?modi:Gdk.Tags.modifier list -> unit
      = "ml_gtk_widget_remove_accelerator"
  external set_accel_path :
      [>`widget] obj -> string -> accel_group -> unit
      = "ml_gtk_widget_set_accel_path"

(*
  external lock_accelerators : [>`widget] obj -> unit
      = "ml_gtk_widget_lock_accelerators"
  external unlock_accelerators : [>`widget] obj -> unit
      = "ml_gtk_widget_unlock_accelerators"
  external accelerators_locked : [>`widget] obj -> bool
      = "ml_gtk_widget_accelerators_locked"
*)
  external window : [>`widget] obj -> Gdk.window
      = "ml_GtkWidget_window"
  external allocation : [>`widget] obj -> rectangle
      = "ml_gtk_widget_allocation"
  external set_colormap : [>`widget] obj -> Gdk.colormap -> unit
      = "ml_gtk_widget_set_colormap"
  external set_visual : [>`widget] obj -> Gdk.visual -> unit
      = "ml_gtk_widget_set_visual"
  external set_default_colormap : Gdk.colormap -> unit
      = "ml_gtk_widget_set_default_colormap"
  external set_default_visual : Gdk.visual -> unit
      = "ml_gtk_widget_set_default_visual"
  external get_default_colormap : unit -> Gdk.colormap
      = "ml_gtk_widget_get_default_colormap"
  external get_default_visual : unit -> Gdk.visual
      = "ml_gtk_widget_get_default_visual"
  external push_colormap : Gdk.colormap -> unit
      = "ml_gtk_widget_push_colormap"
  external push_visual : Gdk.visual -> unit
      = "ml_gtk_widget_push_visual"
  external pop_colormap : unit -> unit
      = "ml_gtk_widget_pop_colormap"
  external pop_visual : unit -> unit
      = "ml_gtk_widget_pop_visual"
  
  (** @since GTK 2.12 *)
  module Tooltip = struct
    external get_markup : [>`widget] obj -> string
      = "ml_gtk_widget_get_tooltip_markup"
    external set_markup : [>`widget] obj -> string -> unit
      = "ml_gtk_widget_set_tooltip_markup"
    external get_text : [>`widget] obj -> string
      = "ml_gtk_widget_get_tooltip_text"
    external set_text : [>`widget] obj -> string -> unit
      = "ml_gtk_widget_set_tooltip_text"
    external get_window : [>`widget] obj -> Gtk.window obj
      = "ml_gtk_widget_get_tooltip_window"
    external set_window : [>`widget] obj -> [>`window] obj -> unit
      = "ml_gtk_widget_set_tooltip_window"
    external get_has_tooltip : [>`widget] obj -> bool
      = "ml_gtk_widget_get_has_tooltip"
    external set_has_tooltip : [>`widget] obj -> bool -> unit
      = "ml_gtk_widget_set_has_tooltip"
    external trigger_query : [>`widget] obj -> unit
      = "ml_gtk_widget_trigger_tooltip_query"
  end

  module Signals = struct
    open GtkSignal
    let marshal f _ = function
      | `OBJECT(Some p) :: _ -> f (cast p)
      |	_ -> invalid_arg "GtkBase.Widget.Signals.marshal"
    let marshal_opt f _ = function
      | `OBJECT(Some obj) :: _ -> f (Some (cast obj))
      | `OBJECT None :: _ -> f None
      | _ -> invalid_arg "GtkBase.Widget.Signals.marshal_opt"
    module Event = struct
      let marshal f argv =
        match Closure.get_args argv with
        | _ :: [`POINTER(Some p)] ->
	    let ev = GdkEvent.unsafe_copy p in
            Closure.set_result argv (`BOOL(f ev))
	| _ -> invalid_arg "GtkBase.Widget.Event.marshal"
      let any : ([>`widget], Gdk.Tags.event_type Gdk.event -> bool) t =
	{ name = "event"; classe = `widget; marshaller = marshal }
      let button_press : ([>`widget], GdkEvent.Button.t -> bool) t =
	{ name = "button_press_event"; classe = `widget;
          marshaller = marshal }
      let button_release : ([>`widget], GdkEvent.Button.t -> bool) t =
	{ name = "button_release_event"; classe = `widget;
          marshaller = marshal }
      let motion_notify : ([>`widget], GdkEvent.Motion.t -> bool) t =
	{ name = "motion_notify_event"; classe = `widget;
          marshaller = marshal }
      let delete : ([>`widget], [`DELETE] Gdk.event -> bool) t =
	{ name = "delete_event"; classe = `widget; marshaller = marshal }
      let destroy : ([>`widget], [`DESTROY] Gdk.event -> bool) t =
	{ name = "destroy_event"; classe = `widget; marshaller = marshal }
      let expose : ([>`widget], GdkEvent.Expose.t -> bool) t =
	{ name = "expose_event"; classe = `widget; marshaller = marshal }
      let key_press : ([>`widget], GdkEvent.Key.t -> bool) t =
	{ name = "key_press_event"; classe = `widget;
          marshaller = marshal }
      let key_release : ([>`widget], GdkEvent.Key.t -> bool) t =
	{ name = "key_release_event"; classe = `widget;
          marshaller = marshal }
      let enter_notify : ([>`widget], GdkEvent.Crossing.t -> bool) t =
	{ name = "enter_notify_event"; classe = `widget;
          marshaller = marshal }
      let leave_notify : ([>`widget], GdkEvent.Crossing.t -> bool) t =
	{ name = "leave_notify_event"; classe = `widget;
          marshaller = marshal }
      let configure : ([>`widget], GdkEvent.Configure.t -> bool) t =
	{ name = "configure_event"; classe = `widget;
          marshaller = marshal }
      let focus_in : ([>`widget], GdkEvent.Focus.t -> bool) t =
	{ name = "focus_in_event"; classe = `widget;
          marshaller = marshal }
      let focus_out : ([>`widget], GdkEvent.Focus.t -> bool) t =
	{ name = "focus_out_event"; classe = `widget;
          marshaller = marshal }
      let map : ([>`widget], [`MAP] Gdk.event -> bool) t =
	{ name = "map_event"; classe = `widget; marshaller = marshal }
      let unmap : ([>`widget], [`UNMAP] Gdk.event -> bool) t =
	{ name = "unmap_event"; classe = `widget; marshaller = marshal }
      let property_notify : ([>`widget], GdkEvent.Property.t -> bool) t =
	{ name = "property_notify_event"; classe = `widget;
          marshaller = marshal }
      let scroll : ([>`widget], GdkEvent.Scroll.t -> bool) t =
	{ name = "scroll_event"; classe = `widget; marshaller = marshal }
      let selection_clear : ([>`widget], GdkEvent.Selection.t -> bool) t =
	{ name = "selection_clear_event"; classe = `widget;
          marshaller = marshal }
      let selection_request : ([>`widget], GdkEvent.Selection.t -> bool) t =
	{ name = "selection_request_event"; classe = `widget;
          marshaller = marshal }
      let selection_notify : ([>`widget], GdkEvent.Selection.t -> bool) t =
	{ name = "selection_notify_event"; classe = `widget;
          marshaller = marshal }
      let proximity_in : ([>`widget], GdkEvent.Proximity.t -> bool) t =
	{ name = "proximity_in_event"; classe = `widget;
          marshaller = marshal }
      let proximity_out : ([>`widget], GdkEvent.Proximity.t -> bool) t =
	{ name = "proximity_out_event"; classe = `widget;
          marshaller = marshal }
      let client : ([>`widget], GdkEvent.Client.t -> bool) t =
	{ name = "client_event"; classe = `widget;
          marshaller = marshal }
      let visibility_notify : ([>`widget], GdkEvent.Visibility.t -> bool) t =
	{ name = "visibility_notify_event"; classe = `widget;
          marshaller = marshal }
      let window_state : ([>`widget], GdkEvent.WindowState.t -> bool) t =
	{ name = "window_state_event"; classe = `widget;
          marshaller = marshal }
    end
  end
end

module Container = struct
  include Container
  let make_params ~cont pl ?border_width =
    Widget.size_params pl ~cont:(fun p ->
      cont (Property.may_cons P.border_width border_width p))

  let children w =
    let l = ref [] in
    foreach w ~f:(fun c -> l := c :: !l);
    List.rev !l
end

module Bin = Bin

module Item = Item

(* Clipboard provides high-level access to Selection *)
module Clipboard = struct
  external get : Gdk.atom -> clipboard = "ml_gtk_clipboard_get"
  external clear : clipboard -> unit = "ml_gtk_clipboard_clear"
  external set_text : clipboard -> string -> unit = "ml_gtk_clipboard_set_text"
  external set_image : clipboard -> GdkPixbuf.pixbuf -> unit
      = "ml_gtk_clipboard_set_image"
  external wait_for_contents : clipboard -> target:Gdk.atom -> selection_data
      = "ml_gtk_clipboard_wait_for_contents"
  external wait_for_text : clipboard -> string option
      = "ml_gtk_clipboard_wait_for_text"
  external wait_for_image : clipboard -> GdkPixbuf.pixbuf option
      = "ml_gtk_clipboard_wait_for_image"
  external wait_for_targets : clipboard -> Gdk.atom list
      = "ml_gtk_clipboard_wait_for_targets"
  external request_contents :
      clipboard -> target:Gdk.atom -> callback:(selection_data -> unit) -> unit
      = "ml_gtk_clipboard_request_contents"
  external request_text :
      clipboard -> callback:(string option -> unit) -> unit
      = "ml_gtk_clipboard_request_text"
end

(* Use of Selection is deprecated: rather use simpler Clipboard module *)
module Selection = struct
  external selection : selection_data -> Gdk.atom
      = "ml_gtk_selection_data_selection"
  external target : selection_data -> Gdk.atom
      = "ml_gtk_selection_data_target"
  external seltype : selection_data -> Gdk.atom
      = "ml_gtk_selection_data_type"
  external format : selection_data -> int
      = "ml_gtk_selection_data_format"
  external get_data : selection_data -> string
      = "ml_gtk_selection_data_get_data"       (* May raise Gpointer.null *)
  external set :
    selection_data -> typ:Gdk.atom -> format:int -> data:string option -> unit
    = "ml_gtk_selection_data_set"

  (* Create a memory-managed copy of the data *)
  external copy : selection_data -> selection_data
      = "ml_gtk_selection_data_copy"

  external owner_set :
    [>`widget] obj -> sel:Gdk.atom -> time:int32 -> bool
    = "ml_gtk_selection_owner_set"
  external add_target :
    [>`widget] obj -> sel:Gdk.atom -> target:Gdk.atom -> info:int -> unit
    = "ml_gtk_selection_add_target"
  external convert :
    [> `widget] obj -> sel:Gdk.atom -> target:Gdk.atom -> time:int32 -> bool
    = "ml_gtk_selection_convert"
  external clear_targets :
    [>`widget] obj -> sel:Gdk.atom -> unit
    = "ml_gtk_selection_clear_targets"
end

module DnD = struct
  external dest_set :
      [>`widget] obj -> flags:dest_defaults list ->
      targets:target_entry array -> actions:Gdk.Tags.drag_action list -> unit 
    = "ml_gtk_drag_dest_set"
  external dest_unset : [>`widget] obj -> unit
      = "ml_gtk_drag_dest_unset"
  external finish :
      Gdk.drag_context -> success:bool -> del:bool -> time:int32 -> unit
      = "ml_gtk_drag_finish"
  external get_data :
      [>`widget] obj -> Gdk.drag_context -> target:Gdk.atom -> time:int32 -> unit
      = "ml_gtk_drag_get_data"
  external get_source_widget : Gdk.drag_context -> widget obj
      = "ml_gtk_drag_get_source_widget"
  external highlight : [>`widget] obj -> unit = "ml_gtk_drag_highlight"
  external unhighlight : [>`widget] obj -> unit = "ml_gtk_drag_unhighlight"
  external set_icon_widget :
      Gdk.drag_context -> [>`widget] obj -> hot_x:int -> hot_y:int -> unit
      = "ml_gtk_drag_set_icon_widget"
  external set_icon_pixmap :
      Gdk.drag_context -> colormap:Gdk.colormap ->
      Gdk.pixmap -> ?mask:Gdk.bitmap -> hot_x:int -> hot_y:int -> unit
      = "ml_gtk_drag_set_icon_pixmap_bc" "ml_gtk_drag_set_icon_pixmap"
  external set_icon_default : Gdk.drag_context -> unit
      = "ml_gtk_drag_set_icon_default"
  external set_default_icon :
      colormap:Gdk.colormap -> Gdk.pixmap ->
      ?mask:Gdk.bitmap -> hot_x:int -> hot_y:int -> unit
      = "ml_gtk_drag_set_default_icon"
  external source_set :
      [>`widget] obj -> ?modi:Gdk.Tags.modifier list ->
      targets:target_entry array -> actions:Gdk.Tags.drag_action list -> unit
      = "ml_gtk_drag_source_set"
  external source_set_icon :
      [>`widget] obj -> colormap:Gdk.colormap ->
      Gdk.pixmap -> ?mask:Gdk.bitmap -> unit
      = "ml_gtk_drag_source_set_icon"
  external source_unset : [>`widget] obj -> unit
      = "ml_gtk_drag_source_unset"
(*  external dest_handle_event : [>`widget] -> *)
end

(** @since GTK 2.12 *)
module Tooltip = struct
  external set_markup : tooltip -> string -> unit
      = "ml_gtk_tooltip_set_markup"
  external set_text : tooltip -> string -> unit
      = "ml_gtk_tooltip_set_text"
  external set_icon : tooltip -> GdkPixbuf.pixbuf -> unit
      = "ml_gtk_tooltip_set_icon"
  external set_icon_from_stock :
      tooltip -> string -> Gtk.Tags.icon_size -> unit
      = "ml_gtk_tooltip_set_icon_from_stock"
  let set_icon_from_stock tt id =
    set_icon_from_stock tt (GtkStock.convert_id id)
  external set_custom : tooltip -> [>`widget] obj -> unit
      = "ml_gtk_tooltip_set_custom"
  external trigger_query :  Gdk.display -> unit
      = "ml_gtk_tooltip_trigger_tooltip_query"
  external set_tip_area : tooltip -> Gdk.Rectangle.t -> unit
      = "ml_gtk_tooltip_set_tip_area"
end
