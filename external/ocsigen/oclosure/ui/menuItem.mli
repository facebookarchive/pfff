(* 
   OClosure Project - 2010
   Class goog.ui.MenuItem
   
   Class representing an item in a menu.
   
   @author : Emmanuel CRESPIN
   @version 0.2
*)

#ifndef UI
open Js
open Component
open Control
open ControlContent
#endif

class type menuItem = object
  inherit control

  (** Sets the menu item to be selectable or not.*)
  method setSelectable : bool t -> unit meth
 
  (*
     Returns the value associated with the menu item. The default implementation
     returns the model object associated with the item (if any), or its caption.
 
  method getValue : unit -> *
  
     Sets the value associated with the menu item.  The default implementation
     stores the value as the model of the menu item. 
  
  method setValue : * -> unit meth
  *)

 (** Sets the menu item to be checkable or not.*)
  method setCheckable : bool t -> unit meth
end

class type ['menuIt] menuItemRenderer = object
  inherit ['menuIt] controlRenderer

(**
   Overrides goog.ui.ControlRenderer#createDom by adding extra markup
   and stying to the menu item's element if it is selectable or checkable.
   @param item Menu item to render.
   @return Root element for the item.
   @override
 *)
  method createDom : 'menuIt t -> Dom_html.element t meth

(**
   Overrides goog.ui.ControlRenderer#decorate by initializing the
   menu item to checkable based on whether the element to be decorated has
   extra stying indicating that it should be.
   @param item Menu item instance to decorate the element.
   @param element Element to decorate.
   @return Decorated element.
   @override
 *)
  method decorate : 'menuIt t -> #Dom_html.element t 
    -> Dom_html.element t meth

(** @return The ARIA role. *)
  method getAriaRole : Gdom.A11y.role_pre optdef meth

(**
   Takes a single goog.ui.Component.State, and returns the
   corresponding CSS class name (null if none).  Overrides the superclass
   implementation by using 'highlight' as opposed to 'hover' as the CSS
   class name suffix for the HOVER state, for backwards compatibility.
   @param state Component state.
   @return CSS class representing the given state
       (undefined if none).
   @override
 *)
  method getClassForState : Component.State.state_pre 
    -> js_string t optdef meth

(** @inheritDoc *)
  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

(** @inheritDoc *)
  method getCssClass : js_string t meth

(**
   Takes a single CSS class name which may represent a component state, and
   returns the corresponding component state (0x00 if none).  Overrides the
   superclass implementation by treating 'goog-option-selected' as special,
   for backwards compatibility.
   @param className CSS class name, possibly representing a component
       state.
   @return state Component state corresponding
       to the given CSS class (0x00 if none).
   @override
 *)
  method getStateFromClass : js_string t -> Component.State.state_pre meth

(**
   Enables/disables checkbox semantics on the menu item.
   @param item Menu item to update.
   @param element Menu item element to update (may be null if the
       item hasn't been rendered yet).
   @param checkable Whether the item should be checkable.
 *)
  method setCheckable : 'menuIt t -> #Dom_html.element t opt -> bool t 
    -> unit meth

(**
   Takes a menu item's root element, and sets its content to the given text
   caption or DOM structure.  Overrides the superclass immplementation by
   making sure that the checkbox structure (for selectable/checkable menu
   items) is preserved.
   @param element The item's root element.
   @param content Text caption or DOM structure to be
       set as the item's content.
   @override
 *)
  method setContent : #Dom_html.element t -> controlContent -> unit meth

(**
   Enables/disables radio button semantics on the menu item.
   @param item Menu item to update.
   @param element Menu item element to update (may be null if the
       item hasn't been rendered yet).
   @param selectable Whether the item should be selectable.
 *)
  method setSelectable : 'menuIt t -> #Dom_html.element t opt -> bool t 
    -> unit meth
end

val menuItemRenderer : (#menuItem menuItemRenderer t) constr

val menuItem : (controlContent -> Gdom.domHelper t opt 
  -> menuItem #menuItemRenderer t opt -> menuItem t) constr
