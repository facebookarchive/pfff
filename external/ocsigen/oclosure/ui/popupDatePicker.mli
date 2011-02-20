(* 
    OClosure Project - 2010 
    Class goog.ui.PopupDatePicker
    
    @author Gabriel Cardoso
    @version 0.2
*)
#ifndef UI
open Component
open Js
open DatePicker
#endif


class type popupDatePicker = object
  inherit component

(**
   Attaches the popup date picker to an element.
   @param element The element to attach to.
 *)
  method attach : #Dom_html.element t -> unit meth

(**
   DatePicker cannot be used to decorate pre-existing html, since they're
   not based on Components.
   @param element Element to decorate.
   @return Returns always false.
 *)
  method canDecorate : #Dom_html.element t -> bool t meth

(** @inheritDoc *)
  method createDom : unit meth

(**
   Detatches the popup date picker from an element.
   @param element The element to detach from.
 *)
  method detach : #Dom_html.element t -> unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(** @inheritDoc *)
  method enterDocument : unit meth

(**
   @return Whether the date picker can automatically move focus to
   its key event target when it is set to visible.
 *)
  method getAllowAutoFocus : bool t meth

(**
   @return The selected date, if any.  See
       goog.ui.DatePicker.getDate().
 *)
  method getDate : Date.date t opt meth

(**
   @return The date picker instance.
 *)
  method getDatePicker : datePicker t meth

(**
   @return The last element that triggered the popup.
 *)
  method getLastTarget : #Dom_html.element t meth

(**
   Hides this popup.
 *)
  method hidePopup : unit meth

(**
   Sets whether the date picker can automatically move focus to its key event
   target when it is set to visible.
   @param allow Whether to allow auto focus.
 *)
  method setAllowAutoFocus : bool t -> unit meth

(**
   Sets the selected date.  See goog.ui.DatePicker.setDate().
   @param date The date to select.
 *)
  method setDate : Date.date t opt -> unit meth

(**
   Show the popup at the bottom-left corner of the specified element.
   @param element Reference element for displaying the popup -- popup
       will appear at the bottom-left corner of this element.
 *)
  method showPopup : #Dom_html.element t -> unit meth
end

(**
   Popup date picker widget.
   @param opt_datePicker Optional DatePicker.  This
       enables the use of a custom date-picker instance.
   @param opt_domHelper Optional DOM helper.
 *)
val popupDatePicker : (datePicker t opt -> Gdom.domHelper t opt
			 -> popupDatePicker t) constr
