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

(* $Id: gdkEvent.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gdk
open Tags

external unsafe_copy : Gpointer.boxed -> [< event_type] event
    = "ml_gdk_event_copy"
external copy : ([< event_type] as 'a) event -> 'a event
    = "ml_gdk_event_copy"
external get_type : 'a event -> 'a = "ml_GdkEventAny_type"
external get_window : 'a event -> window = "ml_GdkEventAny_window"
external get_send_event : 'a event -> bool = "ml_GdkEventAny_send_event"
type timed =
 [ `MOTION_NOTIFY
 | `BUTTON_PRESS | `TWO_BUTTON_PRESS | `THREE_BUTTON_PRESS | `BUTTON_RELEASE
 | `SCROLL
 | `KEY_PRESS | `KEY_RELEASE
 | `ENTER_NOTIFY | `LEAVE_NOTIFY
 | `PROPERTY_NOTIFY
 | `SELECTION_CLEAR | `SELECTION_REQUEST | `SELECTION_NOTIFY
 | `PROXIMITY_IN | `PROXIMITY_OUT
 | `DRAG_ENTER | `DRAG_LEAVE | `DRAG_MOTION | `DRAG_STATUS | `DROP_START
 | `DROP_FINISHED ]
external get_time : [< timed] event -> int32
    = "ml_gdk_event_get_time"

external create : ([< event_type] as 'a) -> 'a event
    = "ml_gdk_event_new"
external set_window : 'a event -> window -> unit
    = "ml_gdk_event_set_window"

type any = event_type event
external unsafe_cast :
  [< event_type] event -> [< event_type] event = "%identity"
let cast ~(kind : ([< event_type] as 'a) list) (ev : any) : 'a event =
  if List.mem (Obj.magic (get_type ev) : [> ]) kind then unsafe_cast ev
  else invalid_arg "GdkEvent.cast"

module Expose = struct
  type t = [ `EXPOSE ] event
  let cast ev : t = cast ev ~kind:[`EXPOSE]
  external area : t -> Rectangle.t = "ml_GdkEventExpose_area"
  external region : t -> region = "ml_GdkEventExpose_region"
  external count : t -> int = "ml_GdkEventExpose_count"
end

module Visibility = struct
  type t = [ `VISIBILITY_NOTIFY ] event
  let cast ev : t = cast ev ~kind:[`VISIBILITY_NOTIFY]
  external visibility : t -> visibility_state
      = "ml_GdkEventVisibility_state"
end

module Motion = struct
  type t = [ `MOTION_NOTIFY ] event
  let cast ev : t = cast ev ~kind:[`MOTION_NOTIFY]
  let time = get_time
  external x : t -> float = "ml_GdkEventMotion_x"
  external y : t -> float = "ml_GdkEventMotion_y"
  external axes : t -> (float * float) option = "ml_GdkEventMotion_axes"
  external state : t -> int = "ml_GdkEventMotion_state"
  external is_hint : t -> bool = "ml_GdkEventMotion_is_hint"
  external device : t -> device = "ml_GdkEventMotion_device"
  external x_root : t -> float = "ml_GdkEventMotion_x_root"
  external y_root : t -> float = "ml_GdkEventMotion_y_root"
end

module Button = struct
  type types =
      [ `BUTTON_PRESS|`TWO_BUTTON_PRESS|`THREE_BUTTON_PRESS|`BUTTON_RELEASE ]
  type t = types event
  let cast ev : t = cast ev
      ~kind:[`BUTTON_PRESS;`TWO_BUTTON_PRESS;
             `THREE_BUTTON_PRESS;`BUTTON_RELEASE]
  let time = get_time
  external x : t -> float = "ml_GdkEventButton_x"
  external y : t -> float = "ml_GdkEventButton_y"
  external axes : t -> (float * float) option = "ml_GdkEventButton_axes"
  external state : t -> int = "ml_GdkEventButton_state"
  external button : t -> int = "ml_GdkEventButton_button"
  external device : t -> device = "ml_GdkEventButton_device"
  external x_root : t -> float = "ml_GdkEventButton_x_root"
  external y_root : t -> float = "ml_GdkEventButton_y_root"
  external set_type : t -> [< types] -> unit
      = "ml_gdk_event_set_type"
  external set_button : t -> int -> unit
      = "ml_gdk_event_button_set_button"
end

module Scroll = struct
  type t = [ `SCROLL ] event
  let cast ev : t = cast ev ~kind:[`SCROLL]
  let time = get_time
  external x : t -> float = "ml_GdkEventScroll_x"
  external y : t -> float = "ml_GdkEventScroll_y"
  external state : t -> int = "ml_GdkEventScroll_state"
  external direction : t -> scroll_direction = "ml_GdkEventScroll_direction"
  external device : t -> device = "ml_GdkEventScroll_device"
  external x_root : t -> float = "ml_GdkEventScroll_x_root"
  external y_root : t -> float = "ml_GdkEventScroll_y_root"
end

module Key = struct
  type t = [ `KEY_PRESS|`KEY_RELEASE ] event
  let cast ev : t = cast ev ~kind:[`KEY_PRESS;`KEY_RELEASE]
  let time = get_time
  external state : t -> int = "ml_GdkEventKey_state"
  external keyval : t -> keysym = "ml_GdkEventKey_keyval"
  external string : t -> string = "ml_GdkEventKey_string"
  external hardware_keycode : t -> int = "ml_GdkEventKey_hardware_keycode"
  external group : t -> int = "ml_GdkEventKey_group"
  let state ev = Convert.modifier (state ev)
end

module Crossing = struct
  type t = [ `ENTER_NOTIFY|`LEAVE_NOTIFY ] event
  let cast ev : t = cast ev ~kind:[`ENTER_NOTIFY;`LEAVE_NOTIFY]
  external subwindow : t -> window = "ml_GdkEventCrossing_subwindow"
  let time = get_time
  external x : t -> float = "ml_GdkEventCrossing_x"
  external y : t -> float = "ml_GdkEventCrossing_y"
  external x_root : t -> float = "ml_GdkEventCrossing_x_root"
  external y_root : t -> float = "ml_GdkEventCrossing_y_root"
  external mode : t -> crossing_mode = "ml_GdkEventCrossing_mode"
  external detail : t -> notify_type = "ml_GdkEventCrossing_detail"
  external focus : t -> bool = "ml_GdkEventCrossing_focus"
  external state : t -> int = "ml_GdkEventCrossing_state"
end

module Focus = struct
  type t = [ `FOCUS_CHANGE ] event
  let cast ev : t = cast ev ~kind:[`FOCUS_CHANGE]
  external focus_in : t -> bool = "ml_GdkEventFocus_in"
end

module Configure = struct
  type t = [ `CONFIGURE ] event
  let cast ev : t = cast ev ~kind:[`CONFIGURE]
  external x : t -> int = "ml_GdkEventConfigure_x"
  external y : t -> int = "ml_GdkEventConfigure_y"
  external width : t -> int = "ml_GdkEventConfigure_width"
  external height : t -> int = "ml_GdkEventConfigure_height"
end

module Property = struct
  type t = [ `PROPERTY_NOTIFY ] event
  let cast ev : t = cast ev ~kind:[`PROPERTY_NOTIFY]
  external atom : t -> atom = "ml_GdkEventProperty_atom"
  let time = get_time
  external state : t -> int = "ml_GdkEventProperty_state"
end

module Selection = struct
  type t = [ `SELECTION_CLEAR|`SELECTION_REQUEST|`SELECTION_NOTIFY ] event
  let cast ev : t = cast ev
      ~kind:[`SELECTION_CLEAR;`SELECTION_REQUEST;`SELECTION_NOTIFY]
  external selection : t -> atom = "ml_GdkEventSelection_selection"
  external target : t -> atom = "ml_GdkEventSelection_target"
  external property : t -> atom = "ml_GdkEventSelection_property"
  external requestor : t -> xid = "ml_GdkEventSelection_requestor"
  let time = get_time
end

module Proximity = struct
  type t = [ `PROXIMITY_IN|`PROXIMITY_OUT ] event
  let cast ev : t = cast ev ~kind:[`PROXIMITY_IN;`PROXIMITY_OUT]
  let time = get_time
  external device : t -> device = "ml_GdkEventProximity_device"
end

module Client = struct
  type t = [ `CLIENT_EVENT ] event
  let cast ev : t = cast ev ~kind:[`CLIENT_EVENT]
  external window : t -> window = "ml_GdkEventClient_window"
  external message_type : t -> atom = "ml_GdkEventClient_message_type"
  external data : t -> xdata_ret = "ml_GdkEventClient_data"
end

module Setting = struct
  type t = [ `SETTING ] event
  let cast ev : t = cast ev ~kind:[`SETTING]
  external action : t -> setting_action = "ml_GdkEventSetting_action"
  external name : t -> string = "ml_GdkEventSetting_name"
end

module WindowState = struct  type t = [ `WINDOW_STATE ] event
  let cast ev : t = cast ev ~kind:[`WINDOW_STATE]
  external changed_mask : t -> int = "ml_GdkEventWindowState_changed_mask"
  external new_window_state : t -> int
      = "ml_GdkEventWindowState_new_window_state"
  let changed_mask ev = Convert.window_state (changed_mask ev)
  let new_window_state ev = Convert.window_state (new_window_state ev)
end
