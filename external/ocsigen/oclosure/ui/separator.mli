(*
   OClosure Project - 2010
   Class goog.ui.Separator, goog.ui.MenuSeparatorRenderer
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open Control
open ControlContent
#endif

class type separator = object
  inherit control
(**
   Configures the component after its DOM has been rendered.  Overrides
   goog.ui.Control#enterDocument by making sure no event handler
   is allocated.
*)
  method enterDocument : unit meth
end

class type ['sep] menuSeparatorRenderer = object
  inherit ['sep] controlRenderer

(**
   Returns an empty, styled menu separator DIV.  Overrides 
   [goog.ui.ControlRenderer#createDom].
   @param separator Separator to render.
   @return Root element for the separator.
*)
  method createDom : 'sep t -> Dom_html.element t meth

(**
   Takes an existing element, and decorates it with the separator.  Overrides
   goog.ui.ControlRenderer#decorate.
   @param separator Separator to decorate the element.
   @param element Element to decorate.
   @return Decorated element.
*)
  method decorate : 'sep t -> #Dom_html.element t -> Dom_html.element t meth

(**
   Returns the CSS class to be applied to the root element of components
   rendered using this renderer.
   @return Renderer-specific CSS class.
*)
  method getCssClass : js_string t meth

(**
   Overrides goog.ui.ControlRenderer#setContent to do nothing, since
   separators are empty.
   @param separator The separator's root element.
   @param content Text caption or DOM structure to be
   set as the separators's content (ignored).
*)
  method setContent : #Dom_html.element t -> controlContent 
    -> unit meth
end

class type menuSeparator = object
  inherit separator
end

val menuSeparator : (Gdom.domHelper t opt -> menuSeparator t) constr

val menuSeparatorRenderer : 
    (#menuSeparator menuSeparatorRenderer t) constr

val separator : (separator #menuSeparatorRenderer t opt -> Gdom.domHelper t opt 
  -> separator t) constr
