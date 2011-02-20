(*
   OClosure Project - 2010
   Class goog.positioning.AbsolutePosition
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef POSITIONING 
open Js
open Corner
open AbstractPosition
#endif
class type absolutePosition = object
  inherit abstractPosition

(**
   Repositions the popup according to the current state.
   
   @param movableElement The DOM element to position.
   @param movableCorner The corner of the movable
   element that should be positioned at the specified position.
   @param opt_margin A margin specified in pixels.
   @param opt_preferredSize Prefered size of the
   movableElement.
 *)
  method reposition : #Dom_html.element t -> Corner.corner -> Math.box t opt -> Math.size t opt -> unit meth
end

(**
   Encapsulates a popup position where the popup absolutely positioned by
   setting the left/top style elements directly to the specified values.
   The position is generally relative to the element's offsetParent. Normally,
   this is the document body, but can be another element if the popup element
   is scoped by an element with relative position.
   
   @param arg1 Left position or coordinate.
   @param opt_arg2 Top position.
*)

val absolutePosition : ((int, Math.coordinate) Tools.Union.t -> int opt ->absolutePosition t) constr

