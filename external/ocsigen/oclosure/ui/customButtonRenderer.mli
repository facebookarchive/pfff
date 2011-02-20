(*
   OClosure Project - 2010
   Class goog.ui.CustomButtonRenderer
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Button
open ControlContent
#endif

class type ['but] customButtonRenderer = object
  inherit ['but] buttonRenderer

  method canDecorate : #Dom_html.element t -> bool t meth

  method createButton : controlContent -> Gdom.domHelper t -> Dom_html.element t meth

  method createDom : 'but t -> Dom_html.element t meth

  method decorate : 'but t -> #Dom_html.element t -> Dom_html.element t meth

  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

  method getCssClass : js_string t meth
end

val customButtonRenderer : (#button customButtonRenderer t) constr

