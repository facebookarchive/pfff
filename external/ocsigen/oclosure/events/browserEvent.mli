(*
   OClosure Project - 2010
   Class goog.events.BrowserEvent
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef EVENTS
open Js
open Event
#endif

module BrowserEvent : sig
  type mouseButton = LEFT | MIDDLE | RIGHT
end

class type browserEvent = object
  inherit event

  method disposeInternal : unit meth

  method getBrowserEvent : Dom_html.event t meth

  method init : #Dom_html.event t -> #Dom.node t opt -> unit meth

  method isButton : BrowserEvent.mouseButton -> bool t meth
  
  method preventDefault : unit meth

  method stopPropagation : unit meth
end

val browserEvent : (#Dom_html.event t opt -> #Dom.node t opt 
  -> browserEvent t) constr
