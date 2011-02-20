(*
   OClosure Project - 2010
   Class goog.events.KeyHandler
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef EVENTS
open Js
open EventTarget
open BrowserEvent
#endif

class type keyHandler = object
  inherit eventTarget

(**
   Adds the proper key event listeners to the element.
   @param element The element to listen on
*)
  method attach : (#Dom_html.element t, #Dom_html.document t) Tools.Union.t 
    -> unit meth

(**
   Removes the listeners that may exist.
*)
  method detach : unit meth

(**
   Disposes of the key handler.
*)
  method disposeInternal : unit meth

(**
   Returns the element listened on for the real keyboard events.
   @return The element listened on for the real keyboard events.
*)
  method getElement : 
      (#Dom_html.element t, #Dom_html.document t) Tools.Union.t opt meth

  method handleEvent : browserEvent t meth
end

(**
   A wrapper around an element that you want to listen to keyboard events on.
   @param opt_element The element or document to listen on
*)
val keyHandler : ((#Dom_html.element t, #Dom_html.document t) Tools.Union.t opt 
		  ->keyHandler t) constr
