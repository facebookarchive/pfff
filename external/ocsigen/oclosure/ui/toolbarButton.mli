(* 
   OClosure Project - 2010
   Class goog.ui.ToolbarButton
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open ControlContent
open Button
open Js
#endif

class type toolbarButton = object
  inherit button
end

val toolbarButton : (controlContent 
		     -> toolbarButton #buttonRenderer t opt 
		     -> Gdom.domHelper t opt -> toolbarButton t) constr 
