(**
   class goog.editor.plugins.UndoRedo

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open UndoRedoManager
#endif
#ifndef EDITOR
open PluginField
#endif

class type undoRedo = object
  inherit plugin

(**
 * Clear the undo/redo stack.
 *)
  method clearHistory : unit meth

(**
 * Disables this plugin for the specified, registered field object.
 * @param fieldObject The field object.
 *)
  method disable : field t -> unit meth

  method disposeInternal : unit meth

(**
 * Enables this plugin for the specified, registered field object. A field
 * object should only be enabled when it is loaded.
 * @param fieldObject The field object.
 *)
  method enable : field t -> unit meth

(*
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
 *
  method execCommand : js_string t*)

(**
 * This is so subclasses can deal with multifield undo-redo.
 * @return Target for COMMAND_VALUE_CHANGE events.
 *)
  method getCurrentEventTarget : field t meth

(**
 * This is so subclasses can deal with multifield undo-redo.
 * @return The active field object for this field. This is
 *     the one registered field object for the single-plugin case and the
 *     focused field for the multi-field plugin case.
 *)
  method getCurrentFieldObject : field t meth

(**
 * This is so subclasses can deal with multifield undo-redo.
 * @param fieldHashCode The Field's hashcode.
 * @return The field object with the hashcode.
 *)
  method getFieldObject : js_string t -> field t meth

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
  method handleKeyboardShortcut : #Events.browserEvent t -> js_string t -> bool t -> bool t meth 

(**
 * Returns whether this plugin is enabled for the field object.
 *
 * @param fieldObject The field object.
 * @return Whether this plugin is enabled for the field object.
 *)
  method isEnabled : field t -> bool t meth

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the string corresponds to a command
 *     this plugin handles.
 *)
  method isSupportedCommand : js_string t -> bool t meth

(*
 * Gets the state of this command if this plugin serves that command.
 * @param command The command to check.
 * @return {*} The value of the command.
 *
  method queryCommandValue : js_strig t*)

(**
 * Refreshes the current state of the editable field as maintained by undo-redo,
 * without adding any undo-redo states to the stack.
 * @param fieldObject The editable field.
 *)
  method refreshCurrentState : field t -> unit meth

(**
 * Registers the field object for use with this plugin.
 * @param fieldObject The editable field object.
 *)
  method registerFieldObject : field t -> unit meth

(*
 * Restores the state of the editable field.
 * @param {goog.editor.plugins.UndoRedo.UndoState_} state The state initiating
 *    the restore.
 * @param content The content to restore.
 * @param {goog.editor.plugins.UndoRedo.CursorPosition_?} cursorPosition
 *     The cursor position within the content.
 *
  method restoreState*)

(**
 * Set the max undo stack depth (not the real memory usage).
 * @param depth Depth of the stack.
 *)
  method setMaxUndoDepth : int -> unit meth
      
(**
 * Set the undo-redo manager used by this plugin. Any state on a previous
 * undo-redo manager is lost.
 * @param manager The undo-redo manager.
 *)
  method setUndoRedoManager : undoRedoManager t -> unit meth

(**
 * Unregisters and disables the fieldObject with this plugin. Thie does *not*
 * clobber the undo stack for the fieldObject though.
 * TODO(user): For the multifield version, we really should add a way to
 * ignore undo actions on field's that have been made uneditable.
 * This is probably as simple as skipping over entries in the undo stack
 * that have a hashcode of an uneditable field.
 * @param fieldObject The field to register with the plugin.
 *)
  method unregisterFieldObject : field t -> unit meth
end

(**
 * Encapsulates undo/redo logic using a custom undo stack (i.e. not browser
 * built-in). Browser built-in undo stacks are too flaky (e.g. IE's gets
 * clobbered on DOM modifications). Also, this allows interleaving non-editing
 * commands into the undo stack via the UndoRedoManager.
 *
 * @param opt_manager An undo redo
 *    manager to be used by this plugin. If none is provided one is created.
 *)
val undoRedo : (undoRedoManager t opt -> undoRedo t) constr
