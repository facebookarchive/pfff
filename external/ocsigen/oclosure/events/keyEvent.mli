(*
   OClosure Project - 2010
   Class goog.events.KeyEvent
   
   @author : 
   @version 0.2
*)
#ifndef EVENTS
open Js
open BrowserEvent
#endif

class type keyEvent = object
  inherit browserEvent
end

val keyEvent : (int -> int -> bool t -> #Dom_html.event t -> keyEvent t) constr 
