(* 
   OClosure Project - 2010
   Class goog.ui.MenuButton
   
   @author Emmanuel CRESPIN
   @version 0.1
*)

#ifndef UI
open Button
open Menu
open MenuItem
open Separator
open ControlContent
open Control
open Js
#endif

class type menuButton = object
  inherit button
  (** Sets up event handlers specific to menu buttons.*)
  method enterDocument : unit meth

 (** Removes event handlers specific to menu buttons, and ensures that the
   attached menu also exits the document.*)
  method exitDocument : unit meth
  
  (** @inheritDoc *)
  method disposeInternal : unit meth
  
  (** Handles mousedown events. Invokes the superclass implementation to dispatch
    an ACTIVATE event and activate the button.  Also toggles the visibility of
    the attached menu.*)
  method handleMouseDown : #Events.event t -> unit meth

  (** Handles mouseup events. Invokes the superclass implementation to dispatch
    an ACTION event and deactivate the button.*)
  method handleMouseUp : #Events.event t -> unit meth
  
  (** Performs the appropriate action when the menu button is activated by the
    user.*)
  method performActionInternal : #Events.event t -> bool t meth
  
  (** Returns true if the given element is to be considered part of the component,
    even if it isn't a DOM descendant of the component's root element.*)
  method containsElement : #Dom_html.element t -> bool t meth
  
  (** @inheritDoc *)
  method handleKeyEventInternal : #Events.event t -> bool t meth
  
  (** Handles [ACTION] events dispatched by an activated menu item.*)
  method handleMenuAction : #Events.event t -> unit meth

  (** Handles [BLUR] events dispatched by the popup menu by closing it.
     Only registered if the menu is focusable.*)
  method handleMenuBlur : #Events.event t -> unit meth
  
  (** Handles blur events dispatched by the button's key event target when it
     loses keyboard focus by closing the popup menu (unless it is focusable).
     Only registered if the button is focusable.*)
  method handleBur : #Events.event t -> unit meth
 
  (** Returns the menu attached to the button.  If no menu is attached, creates
     a new empty menu.*)
  method getMenu : menu t meth
  
  (** Replaces the menu attached to the button with the argument, and returns 
     the previous menu (if any).*)
  method setMenu : menu t -> menu t optdef meth
  
  (** Adds a new menu item at the end of the menu.*)
  method addItem : #control t -> unit meth
  
  (** Adds a new menu item at the specific index in the menu.*)
  method addItemAt : (#menuItem t,#menuSeparator t) Tools.Union.t -> int -> unit meth
  
  (** Removes the item from the menu and disposes of it.*)
  method removeItem : menuItem t -> unit meth

  (** Removes the menu item at a given index in the menu and disposes of it.*)
  method removeItemAt : int -> unit meth
  
  (** Returns the menu item at a given index.*)
  method getItemAt : int  -> menuItem t meth
  
  (** Returns the number of items in the menu (including separators).*)
  method getItemCount : int meth
  
  (** Shows/hides the menu button based on the value of the argument.  Also 
     hides the popup menu if the button is being hidden.*)
  method setVisible : bool t -> bool t opt -> bool t meth
  
  (** Enables/disables the menu button based on the value of the argument, and
     updates its CSS styling.  Also hides the popup menu if the button is being
     disabled.*)
  method setEnabled : bool t -> unit meth
  
  (** @return Whether the menu is aligned to the start of the button
     (left if the render direction is left-to-right, right if the render
     direction is right-to-left).*)
  method isAlignMenuToStart : bool t meth
  
  (** Sets whether the menu is aligned to the start or the end of the button.*)
  method setAlignMenuToStart : bool t -> unit meth
  
  (** Sets whether the menu should scroll when it's too big to fix vertically on
     the screen.  The css of the menu element should have overflow set to auto.*)
  method setScrollOnOverflow : bool t -> unit meth
  
  (** @return Wether the menu will scroll when it's to big to fit
     vertically on the screen.*)
  method isScrollOnOverflow : bool t meth
  
  (** @return Whether the attached menu is focusable.*)
  method isFocusablePopupMenu : bool t meth
  
  (** Sets whether the attached popup menu is focusable.  If the popup menu is
     focusable, it may steal keyboard focus from the menu button, so the button
     will not hide the menu on blur.*)
  method setFocusablePopupMenu : bool t -> unit meth
  
  (** Reveals the menu and hooks up menu-specific event handling.*)
  method showMenu : unit meth
  
  (** Hides the menu and cleans up menu-specific event handling.*)
  method hideMenu : unit meth
  
  (** Opens or closes the attached popup menu.*)
  method setOpen : bool t -> unit meth

  (** Positions the menu under the button.  May be called directly in cases when
     the menu size is known to change.*)
  method positionMenu : unit meth
  
  (** Handles [HIGHLIGHT] events dispatched by the attached menu.*)
  method handleHighlightItem : #Events.event t -> unit meth

  (** Handles UNHIGHLIGHT events dispatched by the associated menu.*)
  method handleUnHighlightItem : #Events.event t -> unit meth
  
end

val menuButton : (controlContent -> menu t opt -> 
  menuButton #buttonRenderer t opt -> Gdom.domHelper t opt -> 
  menuButton t) constr
  
