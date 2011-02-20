(**
   class goog.editor.plugins.TagOnEnterHandler

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open EnterHandler
#endif

class type tagOnEnterHandler = object
  inherit enterHandler

(**
 * @return If true, field will not disable the command
 *     when the field becomes uneditable.
 *)
  method activeOnUneditableFields : bool t meth

(**
 * Gets HTML with no contents that won't collapse, for browsers that
 * collapse the empty string.
 * @return Blank html.
 *)
  method getNonCollapsingBlankHtml : js_string t meth

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Internal backspace handler.
 * @param e The keypress event.
 * @param range The closure range object.
 *)
  method handleBackspaceInternal : #Events.event t -> #Gdom.abstractRange t 
      -> unit meth

(**
 * Internal delete key handler.
 * @param e The keypress event.
 *)
  method handleDeleteGecko : #Events.event t -> unit meth

(**
 * Handle an enter key press on collapsed selection.  handleEnterGecko_ ensures
 * the selection is collapsed by deleting its contents if it is not.  The
 * default implementation does nothing.
 * @param e The key press event.
 * @param wasCollapsed Whether the selection was collapsed before
 *     the key press.  If it was not, code before this function has already
 *     cleared the contents of the selection.
 * @param range Object representing the selection.
 *)
  method handleEnterAtCursorGeckoInternal : #Events.event t -> bool t -> 
    #Gdom.abstractRange t -> unit meth

(**
 * Handle an enter key press in WebKit.
 * @param e The key press event.
 *)
  method handleEnterWebkitInternal : #Events.browserEvent t -> unit meth

(**
 * Internal handler for keyup events.
 * @param e The key event.
 *)
  method handleKeyUpInternal : #Events.event t -> unit meth

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the plugin handles this type of command.
 *)
  method isSupportedCommand : js_string t -> bool t meth

(**
 * Fix paragraphs to be the correct type of node.
 * @param e The <enter> key event.
 * @param split Whether we already split up a blockquote by
 *     manually inserting elements.
 *)
  method processParagraphTagsInternal : #Events.event t -> bool t meth 
    -> unit meth

(**
 * Gets the state of this command if this plugin serves that command.
 * @param command The command to check.
 * @return The value of the command.
 *
  method queryCommandValue : js_string t -> ? *)

end

(**
 * Plugin to handle enter keys. This subclass normalizes all browsers to use
 * the given block tag on enter.
 * @param tag The type of tag to add on enter.
 *)
val tagOnEnterHandler : (Gdom.tagName -> tagOnEnterHandler t) constr
