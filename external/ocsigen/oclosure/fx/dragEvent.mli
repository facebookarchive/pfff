(*
   OClosure Project - 2010
   Class goog.fx.DragEvent
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
open Dragger
#endif

class type dragEvent = object
  inherit Events.event
end

val dragEvent : (js_string t -> dragger t -> int -> int -> 
  Events.browserEvent t -> int opt -> int opt -> bool t opt 
    -> dragEvent t) constr 
