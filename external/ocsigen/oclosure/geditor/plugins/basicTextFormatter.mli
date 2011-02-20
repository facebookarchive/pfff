(**
   class goog.editor.plugins.BasicTextFormatter

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif
open Events

class type basicTextFormatter = object
  inherit plugin

(**
 * Cleans the contents of the node passed to it. The node contents are modified
 * directly, and the modifications will subsequently be used, for operations
 * such as saving the innerHTML of the editor etc. Since the plugins act on
 * the DOM directly, this method can be very expensive.
 *
 * This op is invoked even on disabled plugins.
 *
 * @param fieldCopy The copy of the editable field which
 *     needs to be cleaned up.
 *)
  method cleanContentsDom : #Dom_html.element t -> unit meth

(**
 * Cleans the html contents of Trogedit. Both cleanContentsDom and
 * and cleanContentsHtml will be called on contents extracted from Trogedit.
 * The inverse of prepareContentsHtml.
 *
 * This op is invoked even on disabled plugins.
 *
 * @param originalHtml The trogedit HTML.
 * @return Cleaned-up HTML.
 *)
  method cleanContentsHtml : js_string t -> js_string t meth

/**
 * Handles execCommand. This default implementation handles dispatching
 * BEFORECHANGE, CHANGE, and SELECTIONCHANGE events, and calls
 * execCommandInternal to perform the actual command. Plugins that want to
 * do their own event dispatching should override execCommand, otherwise
 * it is preferred to only override execCommandInternal.
 *
 * This version of execCommand will only work for single field plugins.
 * Multi-field plugins must override execCommand.
 *
 * @param command The command to execute.
 * @param var_args Any additional parameters needed to
 *     execute the command.
 * @return The result of the execCommand, if any.
 */
(*  method execCommandInternal : js_string t -> ? -> ? meth *)

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

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the string corresponds to a command
 *     this plugin handles.
 *)
  method isSupportedCommand : js_string t -> bool t meth

/**
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
 */
(*  method prepareContentsHtml : js_string t -> ? -> js_string t meth*)

(**
 * Gets the command value.
 * @param command The command value to get.
 * @return The current value of the command in the given
 *     selection.  NOTE: This return type list is not documented in MSDN or MDC
 *     and has been constructed from experience.  Please update it
 *     if necessary.
 *)
  method queryCommandValue : js_string t -> js_string t meth
end

(**
 * Functions to style text (e.g. underline, make bold, etc.)
 *)
val basicTextFormatter : basicTextFormatter t constr
