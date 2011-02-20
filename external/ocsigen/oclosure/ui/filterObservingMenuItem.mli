(*
   OClosure Project - 2010
   Class goog.ui.FilterObservingMenuItem
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open MenuItem
open ControlContent
#endif

class type filterObservingMenuItem = object
  inherit menuItem

(**
   Calls the observer function if one has been specified.
   @param opt_str Filter string.
*)
  method callObserver : js_string t opt -> unit meth


(** @inheritDoc *)
  method enterDocument : unit meth

(**
   Sets the observer functions.
   @param f function(goog.ui.FilterObservingMenuItem, string).
*)
  method setObserver : 
      (filterObservingMenuItem t -> js_string t -> unit) callback -> unit meth
end

(**
   Class representing a filter observing menu item.
 
   @param content Text caption or DOM structure to
   display as the content of the item (use to add icons or styling to
   menus).
   @param opt_domHelper Optional DOM helper used for
   document interactions.
   @param opt_renderer Optional renderer.
 *)
val filterObservingMenuItem : (controlContent -> 
  Gdom.domHelper t opt-> filterObservingMenuItem #menuItemRenderer t opt -> 
  filterObservingMenuItem t) constr
