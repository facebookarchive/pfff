(**
   class goog.editor.plugins.EnterHandler
   
   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type enterHandler = object
  inherit plugin
  
(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Handles keypress. It is run before handleKeyboardShortcut and if it returns
 * true handleKeyboardShortcut will not be called.
 * @param e The browser event.
 * @return Whether the event was handled and thus should *not* be
 *     propagated to other plugins or handleKeyboardShortcut.
 *)
  method handleKeyPress : Events.browserEvent t -> bool t meth

(**
 * Handles keyup.
 * @param e The browser event.
 * @return Whether the event was handled and thus should *not* be
 *     propagated to other plugins.
 *)
  method handlerKeyUp : Events.browserEvent t -> bool t meth

(**
 * Prepares the given HTML for editing. Strips out content that should not
 * appear in an editor, and normalizes content as appropriate. The inverse
 * of cleanContentsHtml.
 *
 * This op is invoked even on disabled plugins.
 *
 * @param originalHtml The original HTML.
 * @param styles A map of strings. If the plugin wants to add
 *     any styles to the field element, it should add them as key-value
 *     pairs to this object.
 * @return New HTML that's ok for editing.
 *)
(*  method prepareContentsHtml : js_string t -> ? -> js_string t meth *)
end

(**
 * Plugin to handle enter keys. This does all the crazy to normalize (as much as
 * is reasonable) what happens when you hit enter. This also handles the
 * special casing of hitting enter in a blockquote.
 *
 * In IE and Safari, the resulting HTML uses one DIV tag per line.  In FireFox,
 * the resulting HTML uses BR tags at the end of each line.
 *
 * @constructor
 *)
val enterHandler : enterHandler t constr 
