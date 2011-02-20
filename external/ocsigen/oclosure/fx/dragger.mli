(*
   OClosure Project - 2010
   Class goog.fx.Dragger
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
#endif

class type dragger = object
  inherit Events.eventTarget

(**
   Overridable function for handling the default action of the drag behaviour.
   Normally this is simply moving the element to x,y though in some cases it
   might be used to resize the layer.  This is basically a shortcut to
   implementing a default ondrag event handler.
   @param x X-coordinate for target element.
   @param y Y-coordinate for target element.
*)
  method defaultAction : int -> int -> unit meth

(**
   Tears down the drag object, removes listeners, and nullifies references.
*)
  method disposeInternal : unit meth

(**
   Event handler that is used to end the drag
   @param e Event object.
   @param opt_dragCanceled Whether the drag has been canceled.
*)
  method endDrag : Events.browserEvent t -> bool t opt -> unit meth

 (**
   Event handler that is used to end the drag by cancelling it.
   @param e Event object.
 *)
  method endDragCancel : Events.browserEvent t -> unit meth
      
(**
   Set whether dragger is enabled
   @param enabled Whether dragger is enabled.
*)
  method getEnabled : bool t meth
      
(**
   Returns the event handler, intended for subclass use.
   @return The event handler.
*)
  method getHandler : Events.eventHandler t meth
      
(**
   Gets the distance the user has to drag the element before a drag operation is
   started.
   @return distance The number of pixels after which a mousedown and
   move is considered a drag.
*)
  method getHysteresis : int meth
      
(**
   Returns the 'real' x after limits are applied (allows for some
   limits to be undefined).
   @param x X-coordinate to limit.
   @return The 'real' X-coordinate after limits are applied.
*)
  method limitX : int -> int meth
      
(**
   Returns the 'real' y after limits are applied (allows for some
   limits to be undefined).
   @param y Y-coordinate to limit.
   @return The 'real' Y-coordinate after limits are applied.
*)
  method limitY : int -> int meth
      
(**
   Enables cancelling of built-in IE drag events.
   @param cancelIeDragStart Whether to enable cancelling of IE
   dragstart event.
*)
  method setCancelIeDragStart : bool t -> unit meth
      
(**
   Set whether dragger is enabled
   @param enabled Whether dragger is enabled.
*)
  method setEnabled : bool t -> unit meth
      
(**
   Sets the distance the user has to drag the element before a drag operation is
   started.
   @param distance The number of pixels after which a mousedown and
   move is considered a drag.
*)
  method setHysteresis : int -> unit meth
      
(**
   Sets (or reset) the Drag limits after a Dragger is created.
   @param limits Object containing left, top, width,
   height for new Dragger limits.
*)
  method setLimits : Math.rect t optdef -> unit meth
      
(**
   Sets the SCROLL event target to make drag element follow scrolling.
   
   @param scrollTarget The event target that dispatches SCROLL
   events.
*)
  method setScrollTarget : #Dom_html.eventTarget t -> unit meth
      
(**
   Event handler that is used to start the drag
   @param e Event object.
*)
  method startDrag : Events.browserEvent t -> unit meth
end
         
(**
   A class that allows mouse based dragging (moving) of an element

   @param target The element that will be dragged.
   @param opt_handle An optional handle to control the drag, if null
       the target is used.
   @param opt_limits Object containing left, top, width,
       and height.
 *)
val dragger : (#Dom_html.element t -> #Dom_html.element t opt -> Math.rect t opt
  -> dragger t) constr
