(*
   OClosure Project - 2010
   
   Class goog.ui.ToolbarButtonRenderer
   
   @author Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
open CustomButtonRenderer
open ToolbarButton
#endif

class type ['but] toolbarButtonRenderer = object 
  inherit ['but] customButtonRenderer

  (**
     Returns the CSS class to be applied to the root element of buttons rendered using this renderer.
     @return Renderer-specific CSS class.
  *)
  method getCssClass : js_string t meth

end

(**
   Toolbar-specific renderer for goog.ui.Button, based on goog.ui.CustomButtonRenderer.
*)
val toolbarButtonRenderer : (toolbarButton #toolbarButtonRenderer t) constr
