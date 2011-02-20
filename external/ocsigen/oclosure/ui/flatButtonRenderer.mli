(*
   OClosure Project - 2010
   Class goog.ui.FlatButtonRenderer
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef UI
open Js
open Button
#endif

class type ['but] flatButtonRenderer = object
  inherit ['but] buttonRenderer

(**
   Returns true if this renderer can decorate the element.  Overrides
   goog.ui.ButtonRenderer#canDecorate by returning true if the
   element is a DIV, false otherwise.
   @param element Element to decorate.
   @return Whether the renderer can decorate the element.
 *)
  method canDecorate : #Dom_html.element t -> bool t meth

(**
   Returns the control's contents wrapped in a div element, with
   the renderer's own CSS class and additional state-specific classes applied
   to it, and the button's disabled attribute set or cleared as needed.
   Overrides goog.ui.ButtonRenderer#createDom.
   @param button Button to render.
   @return Root element for the button.
 *)
  method createDom : 'but t -> Dom_html.element t meth


(**
   Takes an existing element and decorates it with the flat button control.
   Initializes the control's ID, content, tooltip, value, and state based
   on the ID of the element, its child nodes, and its CSS classes, respectively.
   Returns the element.  Overrides goog.ui.ButtonRenderer#decorate.
   @param button Button instance to decorate the element.
   @param element Element to decorate.
   @return Decorated element.
 *)
  method decorate : 'but t -> #Dom_html.element t -> Dom_html.element t meth

(**
   Returns the CSS class to be applied to the root element of components
   rendered using this renderer.
   @return Renderer-specific CSS class.
 *)
  method getCssClass : js_string t meth

(**
   Flat buttons can't use the value attribute since they are div elements.
   Overrides goog.ui.ButtonRenderer#getValue to prevent trying to
   access the element's value.
   @param element The button control's root element.
   @return Value not valid for flat buttons.
 *)
  method getValue : #Dom_html.element t -> js_string t opt meth
end

(**
   Flat renderer for goog.ui.Buttons.  Flat buttons can contain
   almost arbitrary HTML content, will flow like inline elements, but can be
   styled like block-level elements.
 *)
val flatButtonRenderer : (#button flatButtonRenderer t) constr
