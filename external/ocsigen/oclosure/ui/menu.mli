(* 
   OClosure Project - 2010
   Class goog.ui.Menu
   
   @author Emmanuel CRESPIN
   @version 0.1
*)

open Js

#ifndef UI
open Container
open MenuItem
open Separator
open Control
#endif

class type menu = object
  inherit container
  method getCssClass : js_string t meth

  (** Returns whether the provided element is to be considered inside the menu for
   * purposes such as dismissing the menu on an event.*)
  method containsElement : #Dom_html.element t -> bool t meth
  
  (** Adds a new menu item at the end of the menu.*)
  method addItem : (#menuItem t, #menuSeparator t) Tools.Union.t -> unit meth
  
  (** Adds a new menu item at a specific index in the menu.*)
  method addItemAt : (#menuItem t, #menuSeparator t) Tools.Union.t -> int -> unit meth
  
  (** Removes an item from the menu and disposes of it.*)
  method removeItem : (#menuItem t, #menuSeparator t) Tools.Union.t  -> unit meth
  
  (** Removes a menu item at a given index in the menu and disposes of it.*)
  method removeItemAt : int -> unit meth
  
  (** Returns a reference to the menu item at a given index.*)
  method getItemAt : int -> (menuItem t, menuSeparator t) Tools.Union.t opt meth
  
  (** Returns the number of items in the menu (including separators).*)
  method getItemCount : int meth
  
  (** Returns the menu items contained in the menu.*)
  method getItems : menuItem t js_array t meth
  
  (** Sets the position of the menu relative to the view port.*)
  method setPosition : (int, Math.coordinate t) Tools.Union.t -> int opt -> unit meth
  
  (** Gets the page offset of the menu, or null if the menu isn't visible*)
  method getPosition : Math.coordinate t opt meth
  
  (** Sets whether the menu can automatically move focus to its key event target
     when it is set to visible.*)
  method setAllowAutoFocus : bool t -> unit meth
  
  (** Return whether the menu can automatically move focus to its key
     event target when it is set to visible.*)
  method getAllowAutoFocus : bool t meth
  
  (** Sets whether the menu will highlight disabled menu items or skip to the next
     active item.*)
  method setAllowHighlightDisabled : bool t -> unit meth
  
  (** Return whether the menu will highlight disabled menu items or skip
     to the next active item.*)
  method getAllowHighlightDisabled : bool t meth
  
  (** Highlights the next item that begins with the specified js_string t.  If no
     (other) item begins with the given js_string t, the selection is unchanged.*)
  method highlightNextPrefix : js_string t -> bool t meth
end

class type ['menu] menuRenderer = object
  inherit ['menu] containerRenderer
	
  method canDecorate : #Dom_html.element t -> bool t meth
 
  method containsElement : #menu t -> #Dom_html.element t -> bool t meth

  method getAriaRole : Gdom.A11y.role_pre optdef meth

  method getCssClass : js_string t meth

  method getDecoratorForChild : #Dom_html.element t -> 
    control t opt meth

  method initializeDom : 'menu t -> unit meth
end

val menuRenderer : (#menu menuRenderer t) constr

val menu : (Gdom.domHelper t opt -> menu #menuRenderer t opt -> menu t) constr

