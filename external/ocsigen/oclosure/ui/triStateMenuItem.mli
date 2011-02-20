(*
   OClosure Project - 2010

   Class goog.ui.TriStateMenuItem
   
   @author : Oran Charles
   @version 0.2
*)

#ifndef UI
open Js
open MenuItem
open TriStateMenuItemRenderer
open ControlContent
#endif
open Gdom
open Events

module TriStateMenuItem : sig

  (**
     Checked states for component.
  *)
  module State : sig
    type t =
	(** Component is not checked. *)
	NOT_CHECKED
	  (** Component is partially checked. *)
      | PARTIALLY_CHECKED
	  (** Component is fully checked. *)
      | FULLY_CHECKED
  end
end

class type triStateMenuItem = object
  inherit menuItem

  (**
     @return The menu item's check state.
  *)
  method getCheckedState : TriStateMenuItem.State.t meth

  (**
     Sets the checked state.
     @param state The checked state.
  *)
  method setCheckedState : TriStateMenuItem.State.t -> unit meth

  (** @inheritDoc *)
  method performActionInternal : event t -> unit meth

end

(**
   Class representing a three state checkbox menu item.
   @param content Text caption or DOM structure to display as the content of the item (use to add icons or styling to menus).
   @param opt_model Data/model associated with the menu item.
   @param opt_domHelper Optional DOM helper used for document interactions.
   @param opt_renderer Optional renderer.
*)

val triStateMenuItem : (controlContent -> domHelper t opt -> 
triStateMenuItem #menuItemRenderer t opt -> triStateMenuItem t ) constr
