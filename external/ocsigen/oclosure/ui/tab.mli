(*
   OClosure Project - 2010

   Class goog.ui.Tab
   
   @author Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
open Control
open ControlContent
#endif

class type tab = object 
  inherit control

  (**
     @return Tab tooltip text (if any).
  *)
  method getTooltip : js_string t optdef meth

  (**
     Sets the tab tooltip text.  If the tab has already been rendered, updates its tooltip.
     @param tooltip New tooltip text.
  *)
  method setTooltip : js_string t -> unit meth

  (**
     Sets the tab tooltip text.  Considered protected; to be called only by the renderer during element decoration.
     @param tooltip New tooltip text.
  *)
  method setTooltipInternal : js_string t -> unit meth

end

class type ['ctrl] tabRenderer = object
  inherit ['ctrl] controlRenderer

  (**
     Returns the CSS class name to be applied to the root element of all tabs
     rendered or decorated using this renderer.
     @return Renderer-specific CSS class name.
     @override
  *)
  method getCssClass : js_string t meth

  (**
     Returns the tab's contents wrapped in a DIV, with the renderer's own CSS
     class and additional state-specific classes applied to it.  Creates the
     following DOM structure:
     <pre>
     <div class="goog-tab" title="Title">Content</div>
     </pre>
     @param tab Tab to render.
     @return Root element for the tab.
  *)
  method createDom : 'ctrl t -> Dom_html.element t meth

  (**
     Decorates the element with the tab.  Initializes the tab's ID, content,
     tooltip, and state based on the ID of the element, its title, child nodes, and CSS classes, respectively.  Returns the element.
     @param tab Tab to decorate the element.
     @param element Element to decorate.
     @return Decorated element.
  *)
  method decorate : 'ctrl t  -> #Dom_html.element t -> Dom_html.element t meth

  (**
     Takes a tab's root element, and returns its tooltip text, or the empty
     string if the element has no tooltip.
     @param element The tab's root element.
     @return The tooltip text (empty string if none).
  *)
  method getTooltip : #Dom_html.element t -> js_string t meth

  (**
     Takes a tab's root element and a tooltip string, and updates the element with the new tooltip.  If the new tooltip is null or undefined, sets the element's title to the empty string.
     @param element The tab's root element.
     @param tooltip New tooltip text (if any).
  *)
  method setTooltip : #Dom_html.element t -> js_string t optdef -> unit meth

end

(**
   Tab control, designed to be hosted in a goog.ui.TabBar.  The tab's DOM may be different based on the configuration of the containing tab bar, so tabs should only be rendered or decorated as children of a tab bar.
   @param content Text caption or DOM structure to display as the tab's caption (if any).
   @param opt_renderer Optional renderer used to render or decorate the tab.
   @param opt_domHelper Optional DOM hepler, used for document interaction.
*)
val tab : (controlContent -> tab #tabRenderer t opt -> Gdom.domHelper t opt -> tab t) constr

(**
   Default renderer for goog.ui.Tab, based on the TabPane code.
*)
val tabRenderer : tab tabRenderer t constr


