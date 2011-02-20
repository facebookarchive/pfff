(*
   OClosure Project - 2010
   Class goog.ui.LinkButtonRenderer
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef UI
open Js
open FlatButtonRenderer
open Button
#endif

class type ['but] linkButtonRenderer = object
  inherit ['but] flatButtonRenderer

  method getCssClass : js_string t meth

end

(**
   Link renderer for goog.ui.Buttons.  Link buttons can contain
   almost arbitrary HTML content, will flow like inline elements, but can be
   styled like block-level elements.
*)
val linkButtonRenderer : (#button linkButtonRenderer t) constr
  
