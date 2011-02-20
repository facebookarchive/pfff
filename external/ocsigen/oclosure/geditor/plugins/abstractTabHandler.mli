(**
   class goog.editor.plugins.AbstractTabHandler

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif
open Events

class type abstractTabHandler = object
  inherit plugin

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Handles keyboard shortcuts.  Preferred to using handleKey* as it will use
 * the proper event based on browser and will be more performant. If
 * handleKeyPress/handleKeyDown returns true, this will not be called. If the
 * plugin handles the shortcut, it is responsible for dispatching appropriate
 * events (change, selection change at the time of this comment). If the plugin
 * calls execCommand on the editable field, then execCommand already takes care
 * of dispatching events.
 * NOTE: For performance reasons this is only called when any key is pressed
 * in conjunction with ctrl/meta keys OR when a small subset of keys (defined
 * in goog.editor.Field.POTENTIAL_SHORTCUT_KEYCODES_) are pressed without
 * ctrl/meta keys. We specifically don't invoke it when altKey is pressed since
 * alt key is used in many i8n UIs to enter certain characters.
 * @param e The browser event.
 * @param key The key pressed.
 * @param isModifierPressed Whether the ctrl/meta key was pressed or
 *     not.
 * @return Whether the event was handled and thus should *not* be
 *     propagated to other plugins. We also call preventDefault on the event if
 *     the return value is true.
 *)
  method handleKeyboardShortcut : browserEvent t -> js_string t -> bool t 
    -> bool t meth
end
