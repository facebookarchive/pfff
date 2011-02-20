(*
   OClosure Project - 2010
   Class goog.ui.Combobox
 
   A ComboBox control.
  
   @author : Cardoso Gabriel
   @version 0.1
*)
#ifndef UI
open Js
open Component
open Control
open MenuItem
#endif

class type comboBox = object
  inherit component

  (**
     Adds a new menu item at the end of the menu.
     @param item Menu item to add to the menu.
  *)
  method addItem : #control t -> unit meth
 
  (**
     Adds a new menu item at a specific index in the menu.
     @param item Menu item to add to the menu.
     @param n Index at which to insert the menu item.
  *)
  method addItemAt : #control t -> int -> unit meth
 
  (** 
     Sets the default text for the combo box.
     @param text The default text for the combo box.
  *)
  method setDefaultText : js_string t -> unit meth

  (** 
     @return text The default text for the combox box.
  *)
  method getDefaultText : js_string t meth

  method enterDocument : unit meth

  method exitDocument : unit meth
 
  (** 
     Combo box currently can't decorate elements.
     @return The value false.
  *)
  method canDecorate : #Dom_html.element t -> bool t meth
  
  method disposeInternal : unit meth
  
  method dissmiss : unit meth
  
  (**
     Removes an item from the menu and disposes it.
     @param item The menu item to remove.
  *)
  method removeItem : #menuItem t -> unit meth
  
  method removeAllItems : unit meth
  
  (** 
     Removes a menu item at a given index in the menu.
  *)
  method removeItemAt : int -> unit meth
  
  (** 
     Returns a reference to the menu item at a given index.
     @param n Index of menu item.
     @return Reference to the menu item.
  *)
  method getItemAt : int -> menuItem t meth
 
  (**
    Sets the field name for the combo box.
    @param fieldName The field name for the combo box.
  *)
  method getFieldName : js_string t meth
 
  (**
    Set to true if a unicode inverted triangle should be displayed in the
    dropdown button.
    This option defaults to false for backwards compatibility.
    @param useDropdownArrow True to use the dropdown arrow.
  *) 
  method setUseDropdownArrow : bool t -> unit meth
 
  (** 
     Sets the current value of the combo box.
     @param value The new value.
  *)
  method setValue : js_string t -> unit meth
 
  (**
     @return The current value of the combo box.
  *)
  method getValue : js_string t meth
end

val comboBox : (Gdom.domHelper t opt -> comboBox t) constr 
