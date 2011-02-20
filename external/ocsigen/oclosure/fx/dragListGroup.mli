(*
   OClosure Project - 2010
   Class goog.fx.DragListGroup
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
#endif

type dragListDirection = DOWN | UP | RIGHT | LEFT

class type dragListGroup = object
  inherit Events.eventTarget

(**
   Adds a drag list to this DragListGroup.
   All calls to this method must happen before the call to init().
   Remember that all child nodes (except text nodes) will be made draggable to
   any other drag list in this group.

   @param dragListElement Must be a container for a list of items
       that should all be made draggable.
   @param growthDirection The direction that this
       drag list grows in (i.e.. if an item is added, the list's bounding box
       expands in this direction).
   @param opt_isDocOrderSameAsGrowthDirection Defaults to true.
       Whether or not the ordering of this drag list's items in the document
       is the same as the list's growth direction.
   @param opt_dragHoverClass CSS class to apply to this drag list when
       the draggerEl hovers over it during a drag action.
 *)
  method addDragList : #Dom_html.element t -> dragListDirection -> bool t opt 
    -> js_string t opt -> unit meth

(**
   Disposes of the DragListGroup.
 *)
  method disposeInternal : unit meth

(**
   Performs the initial setup to make all items in all lists draggable.
 *)
  method init : unit meth

(**
   Sets a user-supplied CSS class to add to the current drag item (during a
   drag action).

   If not set, the default behavior adds visibility:hidden to the current drag
   item so that it is a block of empty space in the hover drag list (if any).
   If this class is set by the user, then the default behavior does not happen
   (unless, of course, the class also contains visibility:hidden).

   @param currDragItemClass The CSS class.
 *)
  method setCurrDragItemClass : js_string t -> unit meth

(**
   Sets a user-supplied CSS class to add to a drag item handle on hover (not
   during a drag action).
   @param dragItemHandleHoverClass The CSS class.
 *)
  method setDragItemHandleHoverClass : js_string t -> unit meth

(**
   Sets a user-supplied CSS class to add to a drag item on hover (not during a
   drag action).
   @param dragItemHoverClass The CSS class.
 *)
  method setDragItemHoverClass : js_string t -> unit meth

 (**
   Sets a user-supplied CSS class to add to the clone of the current drag item
   that's actually being dragged around (during a drag action).
   @param draggerElClass The CSS class.
 *)
 method setDraggerElClass : js_string t -> unit meth

(**
   Sets a user-supplied function used to get the "handle" element for a drag
   item. The function must accept exactly one argument. The argument may be
   any drag item element.

   If not set, the default implementation uses the whole drag item as the
   handle.

   @param getHandleForDragItemFn A function that,
       given any drag item, returns a reference to its "handle" element
       (which may be the drag item element itself).
 *)
  method setFunctionToGetHandlerForDragItem : 
      (#Dom_html.element t -> #Dom_html.element t) callback -> unit meth
end

(**
   A class representing a group of one or more "drag lists" with items that can
   be dragged within them and between them.

   Example usage:
     var dragListGroup = new goog.fx.DragListGroup();
     dragListGroup.setDragItemHandleHoverClass(className1);
     dragListGroup.setDraggerElClass(className2);
     dragListGroup.addDragList(vertList, goog.fx.DragListDirection.DOWN);
     dragListGroup.addDragList(horizList, goog.fx.DragListDirection.RIGHT);
     dragListGroup.init();
 *)
val dragListGroup : (dragListGroup t) constr
