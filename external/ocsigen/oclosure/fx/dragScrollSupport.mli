(*
   OClosure Project - 2010
   Class goog.fx.DragScrollSupport
   
   @author Cardoso Gabriel 
   @version 0.2
*)

#ifndef FX
open Js
#endif

class type dragScrollSupport = object
  inherit Disposable.disposable

(**
   Clean up listeners.
 *)
  method disposeInternal : unit meth

(**
   Handler for mouse moves events.
   @param event Mouse move event.
 *)
  method onMouseMove : #Events.event t -> unit meth

(**
   Sets whether scrolling should be constrained to happen only when the cursor
   is inside the container node.
   NOTE: If a vertical margin is not set, then it does not make sense to
   contain the scroll, because in that case scroll will never be triggered.
   @param constrain Whether scrolling should be constrained to happen
       only when the cursor is inside the container node.
 *)
  method setConstrainScroll : bool t -> unit meth
end

(**
   A scroll support class. Currently this class will automatically scroll
   a scrollable container node and scroll it by a fixed amount at a timed
   interval when the mouse is moved above or below the container or in vertical
   margin areas. Intended for use in drag and drop. This could potentially be
   made more general and could support horizontal scrolling.

   @param containerNode A container that can be scrolled.
   @param opt_verticalMargin Optional vertical margin to use while
       scrolling.
   @param opt_externalMouseMoveTracking Whether mouse move events
       are tracked externally by the client object which calls the mouse move
       event handler, useful when events are generated for more than one source
       element and/or are not real mousemove events.
*)
val dragScrollSupport : (#Dom_html.element t -> int opt -> bool t opt 
  -> dragScrollSupport t) constr
  
