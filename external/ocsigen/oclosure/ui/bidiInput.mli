(*
   OClosure Project - 2010
   Class goog.ui.BidiInput
   
   @author : 
   @version 0.2
*)
#ifndef UI
open Js
open Component
#endif
class type bidiInput = object
  inherit component

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   Returns the direction of the input element.
   @return Return 'rtl' for right-to-left text,
   'ltr' for left-to-right text, or null if the value itself is not
   enough to determine directionality (e.g. an empty value), and the
   direction is inherited from a parent element (typically the body
   element).
*)
  method getDirection : js_string t opt meth
   
(**
   Returns the value of the underlying input field.
   @return Value of the underlying input field.
*)
  method getValue : js_string t meth

(**
   Sets the value of the underlying input field, and sets the direction
   according to the given value.
   @param value  The Value to set in the underlying input field.
*)
  method setValue : js_string t -> unit meth
end

val bidiInput : (Gdom.domHelper t opt -> bidiInput t) constr 
