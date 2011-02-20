(*
   OClosure Project - 2010
   Class goog.fx.DragDropEvent
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
open DragDrop
#endif

class type dragDropEvent = object
  inherit Events.event 

  method disposeInternal : unit meth
end

val dragDropEvent : (js_string t -> #abstractDragDrop t -> 
  dragDropItem t -> #abstractDragDrop t opt -> 
  dragDropItem t opt -> #Dom_html.element t opt -> int opt -> 
  int opt -> int opt -> int opt -> dragDropEvent t) constr
