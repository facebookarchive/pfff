(*
   OClosure Project - 2010
   Class goog.ui.MenuButtonRenderer
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open CustomButtonRenderer
open ControlContent
open MenuButton
#endif

class type ['but] menuButtonRenderer = object
  inherit ['but] customButtonRenderer

(**
   Takes a text caption or existing DOM structure, and returns the content and
   a dropdown arrow element wrapped in a pseudo-rounded-corner box.  Creates
   the following DOM structure:
      <div class="goog-inline-block goog-menu-button-outer-box">
        <div class="goog-inline-block goog-menu-button-inner-box">
          <div class="goog-inline-block goog-menu-button-caption">
            Contents...
          </div>
          <div class="goog-inline-block goog-menu-button-dropdown">
            &nbsp;
          </div>
        </div>
      </div>
   @param content Text caption or DOM structure
       to wrap in a box.
   @param dom DOM helper, used for document interaction.
   @return Pseudo-rounded-corner box containing the content.
 *)
  method createButton : controlContent -> Gdom.domHelper t 
    -> Dom_html.element t meth

(**
   Takes a text caption or existing DOM structure, and returns it wrapped in
   an appropriately-styled DIV.  Creates the following DOM structure:
      <div class="goog-inline-block goog-menu-button-caption">
        Contents...
      </div>
   @param content Text caption or DOM structure
       to wrap in a box.
   @param dom DOM helper, used for document interaction.
   @return Caption element.
 *)
  method createCaption : controlContent -> Gdom.domHelper t 
    -> Dom_html.element t meth

(**
   Returns an appropriately-styled DIV containing a dropdown arrow element.
   Creates the following DOM structure:
      <div class="goog-inline-block goog-menu-button-dropdown">
        &nbsp;
      </div>
   @param dom DOM helper, used for document interaction.
   @return Dropdown element.
 *)
  method createDropdown : Gdom.domHelper t -> Dom_html.element t meth

(**
   Takes an element, decorates it with the menu button control, and returns
   the element.  Overrides goog.ui.CustomButtonRenderer#decorate by
   looking for a child element that can be decorated by a menu, and if it
   finds one, decorates it and attaches it to the menu button.
   @param button Menu button to decorate the element.
   @param element Element to decorate.
   @return Decorated element.
 *)
  method decorate : 'but t -> #Dom_html.element t -> Dom_html.element t meth

(**
   Takes the button's root element and returns the parent element of the
   button's contents.  Overrides the superclass implementation by taking
   the nested DIV structure of menu buttons into account.
   @param element Root element of the button whose content element
       is to be returned.
   @return The button's content element.
 *)
  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

(**
   Returns the CSS class to be applied to the root element of components
   rendered using this renderer.
   @return Renderer-specific CSS class.
 *)
  method getCssClass : js_string t meth

(**
     Takes the menubutton's root element, and sets its content to the given
     text caption or DOM structure. Because the DOM structure of this button is
     conditional based on whether we need to work around FF2/RTL bugs, we
     override the default implementation to take this into account.
     @param element The control's root element.
     @param content Text caption or DOM
         structure to be set as the control's content.
     @override
   *)
  method setContent : #Dom_html.element t -> controlContent 
    -> unit meth
end

val menuButtonRenderer : (#menuButton menuButtonRenderer t) constr

module MenuButtonRenderer : sig
  val getInstance : unit -> #menuButton menuButtonRenderer t 
end
