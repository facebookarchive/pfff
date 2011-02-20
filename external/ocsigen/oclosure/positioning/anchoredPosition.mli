(* 
   OClosure Project - 2010 
   Class goog.positioning.AnchoredPosition
   
   @author Esther Baruk
   @version 0.1
*)

#ifndef POSITIONING
open Js
open Corner
open AbstractPosition
#endif

class type anchoredPosition = object
  inherit abstractPosition
end

val anchoredPosition : (#Dom_html.element t -> Corner.corner opt -> anchoredPosition t) constr
