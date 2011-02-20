(*
   OClosure Project - 2010
   Class goog.ui.ComboBoxItem
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open MenuItem
#endif

class type comboBoxItem = object
  inherit menuItem

  (**
     Sets the menu item to be sticky or not sticky.
     @param sticky Whether the menu item should be sticky.
  *)
  method setSticky : bool t -> unit meth

 (**
    @return Whether the menu item is sticky.
 *)
  method isSticky : unit -> bool t meth

 (**
    Sets the format for a menu item based on a token, bolding the token.
    @param token The token.
 *)
  method setFormatFromToken : js_string t -> js_string t meth
end

val comboBoxItem : (js_string t -> comboBoxItem t) constr
