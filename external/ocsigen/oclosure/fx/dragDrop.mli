(*
   OClosure Project - 2010
   Class goog.fx.DragDrop

   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef FX
open Js
open DragEvent
open Dragger
#endif

class type abstractDragDrop = object
  inherit Events.eventTarget

(**
   Add item to drag object.

   @param item Item to be added.
   @throws Error Thrown if called on instance of abstract class
 *)
  method addItem : dragDropItem t -> unit meth

(**
   Makes drag and drop aware of a target container that could scroll mid drag.
   @param element The scroll container.
 *)
  method addScrollableContainer : #Dom_html.element t -> unit meth

(**
   Associate drop target with drag element.

   @param target Target to add.
 *)
  method addTarget : abstractDragDrop t -> unit meth

(**
   Creates an element for the item being dragged.

   @param sourceEl Drag source element.
   @return The new drag element.
 *)
  method createDragElement : #Dom_html.element t -> Dom_html.element t meth

  method disposeInternal : unit meth

(**
   Event handler that's used to stop drag. Fires a drop event if over a valid
   target.

   @param event Drag event.
 *)
  method endDrag : dragEvent t -> unit meth

(**
   Returns the position for the drag element.

   @param el Drag source element.
   @param dragEl The dragged element created by createDragElement().
   @param event Mouse down event for start of drag.
   @return The position for the drag element.
 *)
  method getDragElementPosition : #Dom_html.element t -> #Dom_html.element t 
    -> Events.browserEvent t -> Math.coordinate t meth

(**
   Returns the dragger object.

   @return The dragger object used by this drag and drop
       instance.
 *)
  method getDragger : dragger t meth

(**
   Initialize drag and drop functionality for sources/targets already added.
   Sources/targets added after init has been called will initialize themselves
   one by one.
 *)
  method init : unit meth

(**
   Whether the control has been initialized.

   @return True if it's been initialized.
 *)
  method isInitialized : bool t meth

(**
   Starts a drag event for an item if the mouse button stays pressed and the
   cursor moves a few pixels. Allows dragging of items without first having to
   register them with addItem.

   @param event Mouse down event.
   @param item Item that's being dragged.
 *)
  method maybeStartDrag : Events.browserEvent t -> dragDropItem t 
      -> unit meth

(**
   Recalculates the geometry of this source's drag targets.  Call this
   if the position or visibility of a drag target has changed during
   a drag, or if targets are added or removed.

   TODO(user): this is an expensive operation;  more efficient APIs
   may be necessary.
 *)
  method recalculateDragTargets : unit meth

(**
   Removes all items.
 *)
  method removeItems : unit meth

(**
   Set class to add to source elements being dragged.

   @param className Class to be added.
 *)
  method setDragClass : js_string t -> unit meth

(**
   Sets the SCROLL event target to make drag element follow scrolling.

   @param scrollTarget The element that dispatches SCROLL events.
 *)
  method setScrollTarget : #Dom_html.eventTarget t -> unit meth

(**
   Set class to add to source elements.

   @param className Class to be added.
 *)
  method setSourceClass : js_string t -> unit meth

(**
   Set class to add to target elements.

   @param className Class to be added.
 *)
  method setTargetClass : js_string t -> unit meth

(**
   Event handler that's used to start drag.

   @param event Mouse move event.
   @param item Item that's being dragged.
 *)
  method startDrag : Events.browserEvent t -> dragDropItem t 
    -> unit meth
end

and dragDropItem = object
  inherit Events.eventTarget

(**
   Gets the element that is currently being dragged.

   @return The element that is currently being dragged.
 *)
  method getCurrentDragElement : #Dom_html.element t meth

(**
   Gets the element that is actually draggable given that the given target was
   attempted to be dragged. This should be overriden when the element that was
   given actually contains many items that can be dragged. From the target, you
   can determine what element should actually be dragged.

   @param target The target that was attempted to be dragged.
   @return The element that is draggable given the target. If
       none are draggable, this will return null.
 *)
  method getDraggableElement : #Dom_html.element t -> #Dom_html.element t meth

(**
   Gets the element that is currently being dragged.

   @return The element that is currently being dragged.
 *)
  method getDraggableElements : #Dom_html.element t js_array t meth

(**
   Sets the dragdrop to which this item belongs.
   @param parent The parent dragdrop.
 *)
  method setParent : abstractDragDrop t -> unit meth
end

class type dragDrop = object
  inherit abstractDragDrop
end

(**
   Class representing a source or target element for drag and drop operations.

   @param element Dom Node, or string representation of node
       id, to be used as drag source/drop target.
   @param opt_data Data associated with the source/target.
 *)
val dragDropItem : ((#Dom_html.element t, js_string t) Tools.Union.t 
		    -> dragDropItem t) constr 

(**
   Abstract class that provides reusable functionality for implementing drag 
   and drop functionality. This class also allows clients to define their own
   subtargeting function so that drop areas can have finer granularity then a
   singe element. This is accomplished by using a client provided function to
   map from element and coordinates to a subregion id. This class can also be
   made aware of scrollable containers that contain drop targets by calling 
   addScrollableContainer. This will cause dnd to take changing scroll 
   positions into account while a drag is occuring. 
*)
val abstractDragDrop : abstractDragDrop t constr

(**
   Drag/drop implementation for creating drag sources/drop targets consisting of
   a single HTML Element.
   
   @param element Dom Node, or string representation of node
   id, to be used as drag source/drop target.
*)
val dragDrop : ((#Dom_html.element t, js_string t) Tools.Union.t 
		-> dragDrop t) constr
