(*
   OClosure Project - 2010
   Class goog.ui.FlatMenuButtonRenderer
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef UI
open Js
open ControlContent
open Button
open FlatButtonRenderer
#endif

class type ['but] flatMenuButtonRenderer = object
  inherit ['but] flatButtonRenderer

(**
   Takes a text caption or existing DOM structure, and returns it wrapped in
   an appropriately-styled DIV.  Creates the following DOM structure:
      <div class="goog-inline-block goog-flat-menu-button-caption">
        Contents...
      </div>
   @param content Text caption or DOM structure to wrap
       in a box.
   @param dom DOM helper, used for document interaction.
   @return Caption element.
 *)
  method createCaption : controlContent -> Gdom.domHelper t 
      -> Dom_html.element t meth

(**
   Returns the button's contents wrapped in the following DOM structure:
      <div class="goog-inline-block goog-flat-menu-button">
          <div class="goog-inline-block goog-flat-menu-button-caption">
            Contents...
          </div>
          <div class="goog-inline-block goog-flat-menu-button-dropdown">
            &nbsp;
          </div>
      </div>
   Overrides.
   @param button Button to render.
   @return Root element for the button.
 *)
  method createDom : 'but t -> Dom_html.element t meth

 (**
   Returns an appropriately-styled DIV containing a dropdown arrow element.
   Creates the following DOM structure:
      <div class="goog-inline-block goog-flat-menu-button-dropdown">
        &nbsp;
      </div>
   @param dom DOM helper, used for document interaction.
   @return Dropdown element.
 *)
 method createDropdown : Gdom.domHelper t -> Dom_html.element t meth

(**
   Takes an element, decorates it with the menu button control, and returns
   the element.  Overrides by
   looking for a child element that can be decorated by a menu, and if it
   finds one, decorates it and attaches it to the menu button.
   @param button Menu button to decorate the element.
   @param element Element to decorate.
   @return Decorated element.
 *)
  method decorate : 'but t -> #Dom_html.element t 
    -> Dom_html.element t meth

(**
   Takes the button's root element and returns the parent element of the
   button's contents.
   @param element Root element of the button whose content
   element is to be returned.
   @return The button's content element (if any).
 *)
  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

(**
   Returns the CSS class to be applied to the root element of components
   rendered using this renderer.
   @return Renderer-specific CSS class.
 *)
  method getCssClass : js_string t meth
end

(**
   Flat Menu Button renderer. Creates a simpler version of
   that doesn't look like a button and
   doesn't have rounded corners. Uses just a <div> and looks more like
   a traditional <select> element.
 *)
val flatMenuButtonRenderer : (#button flatMenuButtonRenderer t) constr
