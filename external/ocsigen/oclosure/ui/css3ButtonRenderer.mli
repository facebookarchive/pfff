(*
   OClosure Project - 2010
   Class goog.ui.Css3ButtonRenderer
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Button
#endif

class type ['but] css3ButtonRenderer = object
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
   Returns the button's contents wrapped in the following DOM structure:
      <div class="goog-inline-block goog-css3-button">
        Contents...
      </div>
   Overrides goog.ui.ButtonRenderer#createDom.
   @param button Button to render.
   @return Root element for the button.
*)
  method createDom : 'but t -> Dom_html.element t meth

(** @inheritDoc *)
  method decorate : 'but t -> #Dom_html.element t -> Dom_html.element t meth

(** @inheritDoc *)
  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

(**
   Returns the CSS class to be applied to the root element of components
   rendered using this renderer.
   @return Renderer-specific CSS class.
*)
  method getCssClass : js_string t meth
end

val css3ButtonRenderer : (#button css3ButtonRenderer t) constr
