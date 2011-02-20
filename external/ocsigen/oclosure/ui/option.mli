(*
   OClosure Project - 2010

   Class goog.ui.Option
   
   @author : Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
open MenuItem
open ControlContent
#endif
open Gdom

class type option = object
  inherit menuItem

(**
   Performs the appropriate action when the option is activated by the user.
   Overrides the superclass implementation by not changing the selection state
   of the option and not dispatching any SELECTED events, for backwards
   compatibility with existing uses of this class.
   @param e Mouse or key event that triggered the action.
   @return True if the action was allowed to proceed, false otherwise.
*)
  method performActionInternal : Events.event t -> bool t meth
 
end

(**
   Class representing a menu option.  This is just a convenience class that
   extends link goog.ui.MenuItem by making it selectable.
   @param content Text caption or DOM structure to display as the content of the item (use to add icons or styling to menus).
   @param  opt_model Data/model associated with the menu item.
   @param opt_domHelper Optional DOM helper used for document interactions.
*)
val option : (controlContent -> bool t opt -> domHelper t opt -> option t ) constr 
