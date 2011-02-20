(*
   OClosure Project - 2010
   Class goog.ui.ColorMenuButton
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open MenuButton
open Menu
open MenuButtonRenderer
open ControlContent
#endif
open Events

class type colorMenuButton = object
  inherit menuButton

(**
   Returns the currently selected color (null if none).
   @return The selected color.
 *)
  method getSelectedColor : js_string t opt meth

(**
   Handles goog.ui.Component.EventType.ACTION events dispatched by
   the menu item clicked by the user.  Updates the button, calls the superclass
   implementation to hide the menu, stops the propagation of the event, and
   dispatches an ACTION event on behalf of the button itself.  Overrides
   goog.ui.MenuButton#handleMenuAction.
   @param e Action event to handle.
 *)
  method handleMenuAction_ : event t -> unit meth

(**
   Opens or closes the menu.  Overrides goog.ui.MenuButton#setOpen by
   generating a default color menu on the fly if needed.
   @param open Whether to open or close the menu.
 *)
  method setOpen : bool t -> unit meth

(**
   Sets the selected color, or clears the selected color if the argument is
   null or not any of the available color choices.
   @param color New color.
 *)
  method setSelectedColor : js_string t opt -> unit meth

(**
   Sets the value associated with the color menu button.  Overrides
   goog.ui.Button#setValue by interpreting the value as a color
   spec string.
   @param color New button value; should be a color spec string.
 *)
  method setValue : js_string t opt -> unit meth
end

(**
   A color menu button control.  Extends goog.ui.MenuButton by adding
   an API for getting and setting the currently selected color from a menu of
   color palettes.
   @param content Text caption or existing DOM
       structure to display as the button's caption.
   @param opt_menu Menu to render under the button when clicked;
       should contain at least one goog.ui.ColorPalette if present.
   @param opt_renderer Button renderer;
       defaults to goog.ui.ColorMenuButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for
       document interaction.
*)
val colorMenuButton : (controlContent -> menu t opt -> 
colorMenuButton #menuButtonRenderer t opt ->
Gdom.domHelper t -> colorMenuButton t) constr
