(*
   OClosure Project - 2010
   Class goog.fx.DragDropGroup
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
open DragDrop
#endif

class type dragDropGroup = object
  inherit abstractDragDrop

(**
   Add DragDropItem to drag object.
   
   @param item DragDropItem being added to the
   drag object.
*)
  method addDragDropItem : #dragDropItem t -> unit meth

(**
   Add item to drag object.
   
   @param element Dom Node, or string representation of node
   id, to be used as drag source/drop target.
*)
  method addItem_ : (#Dom_html.element t, js_string t) Tools.Union.t 
    -> unit meth
end

(**
   Drag/drop implementation for creating drag sources/drop targets consisting of
   multiple HTML Elements (items). All items share the same drop target(s) but
   can be dragged individually.
*)
val dragDropGroup : (dragDropGroup t) constr
