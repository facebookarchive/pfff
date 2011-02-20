(*
   OClosure Project - 2010

   Class goog.ui.SliderBase

   @author Oran Charles
   @version 0.2
 *)

#ifndef UI
open Component
open Js 
#endif

module SliderBase : sig
  (**
     Enum for representing the orientation of the slider.
  *)
  module Orientation : sig
    val _VERTICAL : js_string t
    val _HORIZONTAL : js_string t
  end
end

class type sliderBase = object
  inherit component

  (** @inheritDoc *)
  method createDom : unit meth

  (** @inheritDoc *)
  method decorateInternal : #Dom_html.element t -> unit meth

  (**
     Called when the DOM for the component is for sure in the document.
     Subclasses should override this method to set this element's role.
  *)
  method enterDocument : unit meth
    
  (**
     Moves the thumbs by the specified delta as follows
     - as long as both thumbs stay within [min,max], both thumbs are moved
     - once a thumb reaches or exceeds min (or max, respectively), it stays
     - at min (or max, respectively).
     In case both thumbs have reached min (or max), no change event will fire.
     @param delta The delta by which to move the selected range.
  *)  
  method moveThumbs : float -> unit meth
    
  (**
     Sets the value and extent of the underlying range model. We enforce that
     getMinimum() <= value <= getMaximum() - extent and
     getMinExtent <= extent <= getMaximum() - getValue()
     If this is not satisifed for the given extent, the call is ignored and no
     CHANGE event fires. This is a utility method to allow setting the thumbs
     simultaneously and ensuring that only one event fires.
     @param value The value to which to set the value.
     @param extent The value to which to set the extent.
  *)
  method setValueAndExtent : float -> float -> unit meth

  (**
     @return The minimum value.
  *)
  method getMinimum : float t meth
    
  (**
     Sets the minimum number.
     @param min The minimum value.
  *)
  method setMinimum : float -> unit meth

  (**
     @return The maximum value.
  *)
  method getMaximum : float t meth

  (**
     Sets the maximum number.
     @param max The maximum value.
  *)
  method setMaximum : float -> unit meth

  (**
     @return The value thumb element.
  *)
  method getValueThumb : Dom_html.element t meth

  (**
     @return The extent thumb element.
  *)
  method getExtentThumb : Dom_html.element t meth

  (**
     Changes the orientation.
     @param orient The orientation.
  *)
  method setOrientation : js_string t -> unit meth

  (**
     @return the orientation of the slider.
  *)
  method getOrientation : js_string t meth

  (** @inheritDoc *)
  method disposeInternal : unit meth

  (**
     @return The amount to increment/decrement for page up/down as well
     as when holding down the mouse button on the background.
  *)
  method getBlockIncrement : float t meth

  (**
     Sets the amount to increment/decrement for page up/down as well as when
     holding down the mouse button on the background.
     *
     @param value The value to set the block increment to.
  *)
  method setBlockIncrement : float -> unit meth
    
  (**
     Sets the minimal value that the extent may have.
     *
     @param value The minimal value for the extent.
  *)
  method setMinExtent : float -> unit meth

  (**
     @return The amount to increment/decrement for up, down, left and
     right arrow keys.
  *)
  method getUnitIncrement : float t meth

  (**
     Sets the amount to increment/decrement for up, down, left and right arrow
     keys.
     @param value The value to set the unit increment to.
  *)
  method setUnitIncrement : float -> unit meth

  (**
     @return The step value used to determine how to round the value.
  *)
  method getStep : float opt meth

  (**
     Sets the step value. The step value is used to determine how to round the
     value.
     @param step  The step size.
  *)
  method setStep : float opt -> unit meth
    
  (**
     @return Whether clicking on the backgtround should move directly to
     that point.
  *)
  method getMoveToPointEnabled : bool t meth

  (**
     Sets whether clicking on the background should move directly to that point.
     @param val Whether clicking on the background should move directly to that point.
  *)
  method setMoveToPointEnabled : bool t -> unit meth

  (**
     @return The value of the underlying range model.
  *)  
  method getValue : float t meth
    
  (**
     Sets the value of the underlying range model. We enforce that getMinimum() <= value <= getMaximum() - getExtent()
     If this is not satisifed for the given value, the call is ignored and no CHANGE event fires.
     @param value The value.
  *)
  method setValue : float -> unit meth
    
  (**
     @return The value of the extent of the underlying range model.
  *)
  method getExtent : float t meth
    
  (**
     Sets the extent of the underlying range model. We enforce that getMinExtent() <= extent <= getMaximum() - getValue()
     If this is not satisifed for the given extent, the call is ignored and no
     CHANGE event fires.
     @param extent The value to which to set the extent.
  *)
  method setExtent : float -> unit meth
    
  (**
     Change the visibility of the slider.
     You must call this if you had set the slider's value when it was invisible.
     @param visible Whether to show the slider.
  *)
  method setVisible : bool t -> unit meth

end

(**
   This creates a SliderBase object.
   @param opt_domHelper Optional DOM helper.
*)
val sliderBase : (Gdom.domHelper t opt -> sliderBase t) constr
    
