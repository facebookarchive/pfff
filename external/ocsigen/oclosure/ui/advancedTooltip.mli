(*
   OClosure Project - 2010
   Class goog.ui.AdvancedTooltip
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Tooltip
#endif
#ifndef GOOG
open Gdom
open Tools
#endif


class type advancedTooltip = object
  inherit tooltip
  method getCursorTracking : bool t meth

  method getCursorTrackingHideDelayMs : int meth

  method getHideDelayMs : int meth

  method getHotSpotPadding : Math.box t meth

  method isMouseInTooltip : bool t meth

  method maybeHide : #Dom_html.element t -> unit meth

  method resetHotSpot : unit meth

  method setCursorTracking : bool t -> unit meth

  method setCursorTrackingHideDelayMs : int -> unit meth

  method setHotSpotPadding : Math.box t -> unit meth
end


(**
   Advanced tooltip widget with cursor tracking abilities. Works like a regular
   tooltip but can track the cursor position and direction to determine if the 
   tooltip should be dismissed or remain open.
*)
val advancedTooltip : ((#Dom_html.element t, js_string t) Union.t opt -> js_string t opt -> domHelper t opt -> advancedTooltip t) constr
