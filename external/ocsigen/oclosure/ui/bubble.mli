(* 
    * OClosure Project - 2010
    * Class goog.ui.Bubble
    *
    * The Bubble provides a general purpose bubble implementation 
    * that can be anchored to a particular element and
    * displayed for a period of time.
    * 
    * @author Bozman Cagdas
    * @version 0.1
*)

#ifndef UI
open Js
open Component
#endif

module Bubble : sig
  module Corner : sig
    type corner = 
	TOP_LEFT
      | BOTTOM_LEFT
      | TOP_RIGHT
      | BOTTOM_RIGHT
  end
end

class type bubble = object
  inherit component
  (** Attaches the bubble to an anchor element. 
      Computes the positioning and orientation of the bubble *)
  method attach : #Dom_html.element t -> unit meth

  (** @inheritDoc *)
  method createDom : unit meth

  (** @inheritDoc *)
  method disposeInternal : unit meth

  (** 
     Returns an AnchoredPosition that will position the bubble optimally given 
     the position of the anchor element and the size of the viewport

     @return The AnchoredPosition to give to setPosition (was a 
     goog.Popup.AnchoredPosition in Closure, which is a deprecated class)
  *)
  method getComputedAnchoredPosition : #Dom_html.element t -> Positioning.anchoredPosition t meth
 
  (** Whether the bubble is visible. *)
  method isVisible : bool t meth
  
  (**  Sets whether the bubble should be automatically hidden 
       whenever user clicks outside the bubble element *)
  method setAutoHide : bool t -> unit meth
  
  (**  
     Sets the corner of the bubble to used in the positioning algorithm 
     @param corner Originally of type goog.positioning.Corner in Closure but
     only worked with [TOP|BOTTOM]_[LEFT|RIGHT] (threw an error for other
     constants)
  *)
  method setPinnedCorner : Bubble.Corner.corner -> unit meth

  (**  Sets the position of the bubble.
       Pass null for corner in AnchoredPosition for 
       corner to be computed automatically *)
  method setPosition : Positioning.abstractPosition t -> unit meth
  
  (**  Sets the timeout after which bubble hides itself *)
  method setTimeout : int -> unit meth
  
  (**  Sets whether the bubble should be visible *)
  method setVisible : bool t -> unit meth
end

val bubble : ((js_string t, #Dom_html.element t) Tools.Union.t -> bubble t) constr
