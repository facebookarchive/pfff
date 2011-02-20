(*
   OClosure Project - 2010
   Class goog.ui.ToggleButton
   
   @author : Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open Button
open ControlContent
#endif

class type toggleButton = object
  inherit button
end

(**
   A toggle button, with checkbox-like semantics.  Rendered using
   goog.ui.CustomButtonRenderer by default, though any
   goog.ui.ButtonRenderer would work.
   
   @param content Text caption or existing DOM
   structure to display as the button's caption.
   @param opt_renderer Renderer used to render or
   decorate the button; defaults to goog.ui.CustomButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for
   document interaction.
*)
val toggleButton : (controlContent 
		    -> toggleButton #buttonRenderer t opt 
		    -> Gdom.domHelper t opt -> toggleButton t) constr
