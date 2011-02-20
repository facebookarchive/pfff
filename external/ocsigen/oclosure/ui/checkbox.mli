(* 
   OClosure Project - 2010
   Class goog.ui.CheckBox
   
   3-state checkbox widget. Fires CHECK or UNCHECK events before toggled and
   CHANGE event after toggled by user.
   The checkbox can also be enabled/disabled and get focused and highlighted.
   
   @author : Emmanuel CRESPIN
   @version 0.2
*)

#ifndef UI
open Js
open Control
#endif

class type checkbox = object
  inherit control
  method getChecked : bool t opt meth

    (** Whether the checkbox is checked.*)
  method isChecked : bool t meth
    
    (** Whether the checkbox is enabled. *)
  method isEnabled : bool t meth
  
    (** Whether the checkbox is not checked.*)
  method isUnchecked : bool t meth

    (** Whether the checkbox is in partially checked state.*)
  method isUndetermined : bool t meth
    
    (** Sets the checked state of the checkbox.*)
  method setChecked_ : bool t opt -> unit meth
    
  (**
     Binds an HTML element to the checkbox which if clicked toggles the 
     checkbox. Behaves the same way as the 'label' HTML tag. The label element 
     has to be the direct or non-direct ancestor of the checkbox element because
     it will get the focus when keyboard support is implemented.
  *)
  method setLabel : #Dom_html.element t -> unit meth
    
    (** Toggles the checkbox.*)
  method toggle : unit meth
    
    (** @inheritDoc*)
  method createDom : unit meth
    
    (** @inheritDoc*)
  method decorateInternal : #Dom_html.element t -> unit meth
    
    (** @inheritDoc*)
  method enterDocument : unit meth
    
    (**Fix for tabindex not being updated so that disabled checkbox is not
     * focusable. In particular this fails in Chrome.*)
  method setEnabled : bool t -> unit meth

end

val checkbox : (bool t opt -> checkbox t) constr 
