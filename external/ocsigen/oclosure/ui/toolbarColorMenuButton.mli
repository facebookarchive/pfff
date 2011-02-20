(*
   OClosure Project - 2010

   Class goog.ui.ToolbarColorMenuButton
   Class goog.ui.ToolbarColorMenuButtonRenderer
   
   @author Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
open ControlContent
open ToolbarMenuButtonRenderer
open ColorMenuButton
open ColorMenuButtonRenderer
open Menu
#endif

class type toolbarColorMenuButton = object
  inherit colorMenuButton
end

class type ['but] toolbarColorMenuButtonRenderer = object
  inherit ['but] toolbarMenuButtonRenderer

  (**
     Overrides the superclass implementation by wrapping the caption text or DOM structure in a color indicator element.  Creates the following DOM structure:
     <div class="goog-inline-block goog-toolbar-menu-button-caption">
     <div class="goog-color-menu-button-indicator">
     Contents...
     </div>
     </div>
     @param content Text caption or DOM structure.
     @param dom DOM helper, used for document interaction.
     @return Caption element.
  *)
  method createCaption : controlContent -> Gdom.domHelper t -> Dom_html.element t meth

  (**
     Takes a color menu button control's root element and a value object
     (which is assumed to be a color), and updates the button's DOM to reflect the new color.  Overrides goog.ui.ButtonRenderer#setValue.
     @param element The button control's root element (if rendered).
     @param value New value; assumed to be a color spec string.
  *)
  method setValue : #Dom_html.element t -> js_string t -> unit meth

  (**
     Initializes the button's DOM when it enters the document.  Overrides the superclass implementation by making sure the button's color indicator is initialized.
     @param button Button whose DOM is to be initialized as it enters the document.
  *)
  method initializeDom : colorMenuButton t -> unit meth

end

(**
   A color menu button control for a toolbar.
   @param content Text caption or existing DOM structure to display as the button's caption.
   @param opt_menu Menu to render under the button when clicked; should contain at least one goog.ui.ColorPalette if present.
   @param opt_renderer Optional renderer used to render or decorate the button; defaults to goog.ui.ToolbarColorMenuButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for
   document interaction.
*)
val toolbarColorMenuButton : (controlContent -> menu t opt 
  -> colorMenuButton #colorMenuButtonRenderer t 
  -> Gdom.domHelper t -> toolbarColorMenuButton t) constr 

(**
   Toolbar-style renderer for goog.ui.ColorMenuButton.
*)
val toolbarColorMenuButtonRenderer : (toolbarColorMenuButton #toolbarColorMenuButtonRenderer t) constr
  
