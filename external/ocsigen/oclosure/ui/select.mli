(*
   OClosure Project - 2010

   Class goog.ui.Select
   
   @author : Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
open ControlContent
open Menu
open MenuButton
open MenuItem
open Separator
open Button
#endif
open Gdom
open Events

class type select = object
  inherit menuButton

  (**
     Configures the component after its DOM has been rendered, and sets up event
     handling.  Overrides goog.ui.MenuButton#enterDocument.
  *)
  method enterDocument : unit meth
    
  (**
     Decorates the given element with this control.  Overrides the superclass
     implementation by initializing the default caption on the select button.
     @param element Element to decorate.
  *)
  method decorateInternal : #Dom_html.element t -> unit meth

  (** @inheritDoc *)
  method disposeInternal : unit meth

  (**
     Handles goog.ui.Component.EventType.ACTION events dispatched by
     the menu item clicked by the user.  Updates the selection model, calls
     the superclass implementation to hide the menu, stops the propagation of
     the event, and dispatches an ACTION event on behalf of the select control
     itself.  Overrides goog.ui.MenuButton#handleMenuAction.
     @param e Action event to handle.
  *)
  method handleMenuAction : #event t -> unit meth

  (**
     Handles goog.events.EventType.SELECT events raised by the
     selection model when the selection changes.  Updates the contents of the
     select button.
     @param e Selection event to handle.
  *)
  method handleSelectionChange : #event t -> unit meth

  (**
     Replaces the menu currently attached to the control (if any) with the given
     argument, and updates the selection model.  Does nothing if the new menu is
     the same as the old one.  Overrides goog.ui.MenuButton#setMenu.
     @param menu New menu to be attached to the menu button.
     @return Previous menu (undefined if none).
  *)
  method setMenu : menu t -> menu t optdef meth

  (**
     Returns the default caption to be shown when no option is selected.
     @return Default caption.
  *)
  method getDefaultCaption : controlContent meth

  (**
     Sets the default caption to the given string or DOM structure.
     @param caption Default caption to be shown
     when no option is selected.
  *)
  method setDefaultCaption : controlContent -> unit meth

  (**
     Adds a new menu item at the end of the menu.
     @param item Menu item to add to the
     menu.
  *)
  method addItem_ : (#menuItem t, #menuSeparator t) Tools.Union.t -> unit meth

  (**
     Adds a new menu item at a specific index in the menu.
     @param item Menu item to add to the
     menu.
     @param index Index at which to insert the menu item.
  *)
  method addItemAt : (#menuItem t, #menuSeparator t) Tools.Union.t -> int -> unit meth

  (**
     Removes an item from the menu and disposes it.
     @param item The menu item to remove.
  *)
  method removeItem : menuItem t -> unit meth

  (**
     Removes a menu item at a given index in the menu and disposes it.
     @param index Index of item.
  *)
  method removeItemAt : int -> unit meth

  (**
     Selects the specified option (assumed to be in the select menu), and
     deselects the previously selected option, if any.  A null argument clears
     the selection.
     @param item Option to be selected (null to clear
     the selection).
  *)
  method setSelectedItem : menuItem t -> unit meth

  (**
     Selects the option at the specified index, or clears the selection if the
     index is out of bounds.
     @param index Index of the option to be selected.
  *)
  method setSelectedIndex : int -> unit meth

  (*(**
    Selects the first option found with an associated value equal to the
    argument, or clears the selection if no such option is found.  A null
    argument also clears the selection.  Overrides goog.ui.Button#setValue.
    @param value Value of the option to be selected (null to clear
    the selection).
  *)
    method setValue : *)

  (**
     Returns the currently selected option.
     @return The currently selected option (null if none).
  *)
  method getSelectedItem : menuItem t meth

  (**
     Returns the index of the currently selected option.
     @return 0-based index of the currently selected option (-1 if none).
  *)
  method getSelectedIndex : int meth

  (**
     Opens or closes the menu.  Overrides goog.ui.MenuButton#setOpen by
     highlighting the currently selected option on open.
     @param open Whether to open or close the menu.
  *)
  method setOpen : bool t -> unit meth

end

(**
   A selection control.  Extends goog.ui.MenuButton by composing a
   menu with a selection model, and automatically updating the button's caption based on the current selection.
   @param caption Default caption or existing DOM structure to display as the button's caption when nothing is selected.
   @param opt_menu Menu containing selection options.
   @param opt_renderer Renderer used to render or decorate the control; defaults to goog.ui.MenuButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for document interaction.
*)
val select : (controlContent opt -> menu t opt 
  -> select #buttonRenderer t opt -> domHelper t opt -> select t) constr
