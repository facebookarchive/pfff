(*
   OClosure Project - 2010

   Class goog.ui.ToolbarMenuButtonRenderer
   
   @author Oran Charles
   @version 0.2
*)

#ifndef UI
open Js
open MenuButtonRenderer
open ToolbarMenuButton
#endif

class type ['but] toolbarMenuButtonRenderer = object
  inherit ['but] menuButtonRenderer

  (**
     Returns the CSS class to be applied to the root element of menu buttons rendered using this renderer.
     @return Renderer-specific CSS class.
  *)
  method getCssClass : js_string t meth

end

(**
   Toolbar-specific renderer for goog.ui.MenuButton, based on goog.ui.MenuButtonRenderer.
*)
val toolbarMenuButtonRenderer : (toolbarMenuButton #toolbarMenuButtonRenderer t) constr
