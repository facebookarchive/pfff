(*
   OClosure Project - 2010

   Class goog.ui.ProgressBar
   
   @author : Oran Charles
   @version 0.2
*)
#ifndef UI
open Component
open Js
#endif

class type progressBar = object
  inherit component
    (** Creates the DOM nodes needed for the progress bar. *)
  method createDom : unit meth

  (** Called when the DOM for the component is for sure in the document. *)
  method enterDocument : unit meth
    
  (** Called when the DOM for the component is for sure in the document. *)
  method exitDocument : unit meth

  (** Decorates an existing HTML DIV element as a progress bar input. If the element contains a child with a class name of 'progress-bar-thumb' that will be used as the thumb. 
      @param element  The HTML element to decorate.*)
  method decorateInternal : #Dom_html.element t -> unit meth

  (** @return The value. *)
  method getValue : float t meth

  (** Sets the value. 
      @param v The value.*)
  method setValue : float -> unit meth

  (** @return The minimum value. *)
  method getMinimum : float t meth

  (** Sets the minimum number. 
      @param v The minimum value.*)
  method setMinimum : float -> unit meth

  (** @return The maximum value. *)
  method getMaximum : float t meth
    
  (** Sets the maximum number. 
      @param v The maximum value.*)
  method setMaximum : float -> unit meth

  (** Changes the orientation. 
      @param orient The orientation.*)
  method setOrientation : js_string t -> unit meth

  (** @param orient The orientation. *)
  method getOrientation : js_string t meth
    
  (** @inheritDoc *)
  method disposableInternal : unit meth
    
  (** @return The step value used to determine how to round the value. *)
  method getStep : int meth

  (** Sets the step value. The step value is used to determine how to round the value.
      @param step  The step size. *)
  method setStep : int -> unit meth

end

val progressBar : progressBar t constr

module ProgressBar : sig

  (** Enum for representing the orientation of the progress bar. *)
  module Orientation : sig
    val _VERTICAL : js_string t
    val _HORIZONTAL : js_string t
  end
end
