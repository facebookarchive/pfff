(**
   class goog.editor.plugins.RemoveFormatting

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type removeFormatting = object
  inherit plugin

(*  method execCommandInternal : js_string t -> ? -> ? *)

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Handle per node special processing if neccessary. If this function returns
 * null then standard cleanup is applied. Otherwise this node and all children
 * are assumed to be cleaned.
 * NOTE(user): If an alternate RemoveFormatting processor is provided
 * (setRemoveFormattingFunc()), this will no longer work.
 * @param node The node to clean.
 * @return The HTML strig representation of the cleaned data.
 *)
  method getValueForNode : #Dom.node t -> js_string t opt meth

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
  method handleKeyboardShortcut : Events.browserEvent t -> js_string t -> bool t
    -> bool t meth 

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the plugin handles this type of command.
 *)
  method isSupportedCommand : js_string t -> bool t meth

(**
 * Sets a function to be used for remove formatting.
 * @param removeFormattingFunc - A function that
 *     takes  a string of html and returns a string of html that does any other
 *     formatting changes desired.  Use this only if trogedit's behavior doesn't
 *     meet your needs.
 *)
  method setRemoveFormattingFunc : (js_string t -> js_string t) callback 
    -> unit meth
end

(**
 * A plugin to handle removing formatting from selected text.
 *)
val removeFormatting : removeFormatting t constr
