(* 
   OClosure Project - 2010
   Class goog.ui.Dialog

   @author Cardoso Gabriel
   @author Bozman Cagdas
   @version 0.2
*)
#ifndef UI
open Component
open Js
#endif

class type dialog = object
  inherit component
  (** Allows arbitrary HTML to be set in the content element. *)
  method setContent : js_string t -> unit meth
 
  (** Sets the title. *)
  method setTitle : js_string t -> unit meth

  (** Sets the visibility of the dialog box and moves focus to the default 
     button. 
     Lazily renders the component if needed. 
   *)
  method setVisible : bool t -> unit meth
end

(**
   @param opt_class CSS class name for the dialog element, also used
   as a class name prefix for related elements; defaults to modal-dialog.
   @param opt_useIframeMask Work around windowed controls z-index
   issue by using an iframe instead of a div for bg element.
   @param opt_domHelper Optional DOM helper; see goog.ui.Component for 
   semantics.
*)
val dialog : (js_string opt -> bool t opt -> Gdom.domHelper t opt -> dialog t) constr
