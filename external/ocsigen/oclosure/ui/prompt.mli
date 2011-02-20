(* 
   OClosure Project - 2010
   Class goog.ui.Prompt
   
   @author Bozman Cagdas 
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef UI
open Dialog
open Js
#endif

class type prompt = object
  inherit dialog
  (** Return the number of cols in the user input element. *)
  method getCols : int meth
 
 (** Return the number of rows in the user input element. *)
  method getRows : int meth
 
 (** Set the number of cols in the user input element. *)
  method setCols : int -> unit meth
 
 (** Sets the default value of the prompt when it is displayed. *)
  method setDefaultValue : js_string t -> unit meth
 
 (** Set the number of rows in the user input element. *)
  method setRows : int -> unit meth

 (** Causes the prompt to appear, centered on the screen, gives focus
     to the text box, and selects the text *)
  method setVisible : bool t -> unit meth

end

val prompt : (js_string t -> js_string t -> (js_string t opt -> bool t) callback
  -> prompt t) constr
  
