(* 
   Oclosure Project - 2010
   Class goog.ui.SubMenu
   
   @author : Emmanuel CRESPIN
   @version 0.1
*)

#ifndef UI
open Menu
open MenuItem
open Component
open ControlContent
open Event
open Js
#endif

class type subMenu = object
  inherit menuItem
  (** @inheritDoc *)
  method enterDocument : unit meth

  (** @inheritDoc *)
  method exitDocument : unit meth

  (** @inheritDoc *)
  method disposeInternal : unit meth

  (** Dismisses the submenu on a delay, with the result that the user needs less
   * accuracy when moving to submenus.  Alternate implementations could use
   * geometry instead of a timer.*)
  method setHighlighted : bool t -> unit meth

  (** Show the submenu and ensure that all siblings are hidden.*)
  method showSubMenu : unit meth

  (** Dismisses the menu and all further submenus.*)
  method dismissSubMenu : unit meth

  (** Clears the show and hide timers for the sub menu.*)
  method clearTimers : unit meth

  (** Sets the menu item to be visible or invisible.*)
  method setVisible : bool t -> bool t opt -> bool t meth

  (**
     Handles a key event that is passed to the menu item from its parent because
     it is highlighted.  If the right key is pressed the sub menu takes control
     and delegates further key events to its menu until it is dismissed OR the
     left key is pressed.*)
  method handleKeyEvent : #Events.event t -> bool t meth

  (** 
     Sets a timer to show the submenu and then dispatches an ENTER event to the
     parent menu.*)
  method handleMouseOver : #Events.event t -> unit meth

  (** Overrides the default mouseup event handler, so that the ACTION isn't
    dispatched for the submenu itself, instead the submenu is shown instantly.*)
  method performActionInternal : #Events.event t  -> bool t meth

  (** Sets whether the submenu is aligned at the end of the parent menu.*)
  method setAlignToEnd : bool t -> unit meth

  (** Determines whether the submenu is aligned at the end of the parent menu.*)
  method isAlignedToEnd : bool t meth

  (** Adds a new menu item at the end of the menu.*)
  method addItem : menuItem t -> unit meth

  (** Adds a new menu item at a specific index in the menu.*)
  method addItemAt : menuItem t -> float -> unit meth

  (** Removes an item from the menu and disposes it.*)
  method removeItem : menuItem t -> unit meth

  (** Removes a menu item at a given index in the menu and disposes it.*)
  method removeItemAt : float -> unit meth

  (** Returns a reference to the menu item at a given index.*)
  method getItemAt : float -> component meth

  (** Returns the number of items in the sub menu (including separators).*)
  method getItemCount : float t meth

  (** Returns the menu items contained in the sub menu.*)
  method getItems : menuItem t js_array t meth

  (** Gets a reference to the submenu's actual menu.*)
  method getMenu : menu t meth

  (** Sets the submenu to a specific menu.*)
  method setMenu : menu t -> unit meth
  
  (** Returns true if the provided element is to be considered inside the menu for
   * purposes such as dismissing the menu on an event.  This is so submenus can
   * make use of elements outside their own DOM.*)
  method containsElement : #Dom_html.element t -> bool t meth

  (** @param isAdjustable Whether this submenu is adjustable.*)
  method setPositionAdjustable : bool t -> unit meth

  (** @return Whether this submenu is adjustable.*)
  method isPositionAdjustable : bool t meth
end

val subMenu : (controlContent -> Gdom.domHelper t opt -> subMenu #menuItemRenderer t opt -> subMenu t) constr
