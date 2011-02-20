(*
   OClosure Project - 2010
   Class goog.ui.CheckBoxMenuItem
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open ControlContent
open MenuItem
#endif

class type checkBoxMenuItem = object
  inherit menuItem
end

(**
   Class representing a checkbox menu item.  This is just a convenience class
   that extends goog.ui.MenuItem by making it checkable.
   
   @param content Text caption or DOM structure to
   display as the content of the item (use to add icons or styling to
   menus).
*)
val checkBoxMenuItem : (controlContent -> checkBoxMenuItem t) constr
