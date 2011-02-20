(**
   class goog.editor.plugins.AbstractBubblePlugin

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif
open Gdom
open Events

class type abstractBubblePlugin = object
  inherit plugin

(**
 * Closes the bubble.
 *)
  method closeBubble : unit meth

(**
 * Creates and shows the property bubble.
 * @param targetElement The target element of the bubble.
 *)
  method createBubble : #Dom_html.element t -> unit meth

(** @inheritDoc *)
  method disable : field t -> unit meth

(**
 * @return The dom helper for the bubble window.
 *)
  method getBubbleDom : domHelper t meth

(**
 * Should be overriden by subclasses to return the bubble target element or
 * null if an element of their required type isn't found.
 * @param selectedElement The target of the selection change event or
 *     the parent container of the current entire selection.
 * @return The HTML bubble target element or null if no element of
 *     the required type is not found.
 *)
  method getBubbleTargetFromSelection : 
      #Dom_html.element t -> Dom_html.element t opt meth

(**
 * Returns the element whose properties the bubble manipulates.
 * @return The target element.
 *)
  method getTargetElement : Dom_html.element t meth

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Handles keyup.
 * @param e The browser event.
 * @return Whether the event was handled and thus should *not* be
 *     propagated to other plugins.
 *)
  method handleKeyUp : browserEvent t -> bool t meth

(**
 * Pops up a property bubble for the given selection if appropriate and closes
 * open property bubbles if no longer needed.  This should not be overridden.
 * @param opt_e Optional selectionchange event that
 *     initiated this handler.
 * @return Whether we handled the selection change.
 *)
  method handleSelectionChange : event t opt -> bool t meth

(**
 * @return Whether the bubble is visible.
 *)
  method isVisible : bool t meth

(**
 * Reposition the property bubble.
 *)
  method reposition : unit meth

(**
 * Sets the bubble parent.
 * @param bubbleParent An element where the bubble will be
 *     anchored. If null, we will use the application document. This
 *     is useful when you have an editor embedded in a scrolling div.
 *)
  method setBubbleParent : #Dom_html.element t -> unit meth
end
