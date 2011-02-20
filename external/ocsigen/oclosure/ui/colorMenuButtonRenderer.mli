(*
   OClosure Project - 2010
   Class goog.ui.ColorMenuButtonRenderer
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open MenuButtonRenderer
open ColorMenuButton
open ControlContent
#endif

class type ['but] colorMenuButtonRenderer = object
  inherit ['but] menuButtonRenderer

(**
   Overrides the superclass implementation by wrapping the caption text or DOM
   structure in a color indicator element.  Creates the following DOM structure:
     <div class="goog-inline-block goog-menu-button-caption">
       <div class="goog-color-menu-button-indicator">
         Contents...
       </div>
     </div>
   The 'goog-color-menu-button-indicator' style should be defined to have a
   bottom border of nonzero width and a default color that blends into its
   background.
   @param content Text caption or DOM structure.
   @param dom DOM helper, used for document interaction.
   @return Caption element.
 *)
  method createCaption : controlContent -> Gdom.domHelper t -> Dom_html.element t meth

(**
   Initializes the button's DOM when it enters the document.  Overrides the
   superclass implementation by making sure the button's color indicator is
   initialized.
   @param button Button whose DOM is to be
       initialized as it enters the document.
 *)
  method initializeDom : 'but t -> unit meth

(**
   Takes a color menu button control's root element and a value object
   (which is assumed to be a color), and updates the button's DOM to reflect
   the new color.  Overrides goog.ui.ButtonRenderer#setValue.
   @param element The button control's root element (if rendered).
   @param value New value; assumed to be a color spec string.
 *)
  method setValue : #Dom_html.element t -> js_string t -> unit meth
end

val colorMenuButtonRenderer : (colorMenuButton colorMenuButtonRenderer t) constr
