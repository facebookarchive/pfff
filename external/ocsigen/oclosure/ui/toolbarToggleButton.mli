(*
   OClosure Project - 2010
   Class goog.ui.ToolbarToggleButton
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open ToggleButton
open ToolbarButtonRenderer
open ControlContent
#endif

class type toolbarToggleButton = object
  inherit toggleButton
end

(**
   A toggle button control for a toolbar.   
   @param content Text caption or existing DOM
   structure to display as the button's caption.
   @param opt_renderer Renderer used to render or
   decorate the button; defaults to goog.ui.ToolbarButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for
   document interaction.
*)
val toolbarToggleButton : 
    (controlContent 
     -> toolbarToggleButton #toolbarButtonRenderer t opt 
     -> Gdom.domHelper t opt -> toolbarToggleButton t) constr
