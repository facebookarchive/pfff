(**
   class goog.editor.plugins.TableEditor

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type tableEditor = object
  inherit plugin

(**
   Adds a function to filter out non-user-editable tables.
*)
  method addIsTableEditableFunction : (#Dom_html.element t -> bool t) callback
      -> unit meth

(**
 * Enables this plugin for the specified, registered field object. A field
 * object should only be enabled when it is loaded.
 * @param fieldObject The field object.
 *)
  method enable : field t -> unit meth

(**
 * Handles execCommand. This default implementation does nothing, and is
 * called by execCommand, which handles event dispatching. This method should
 * be overriden by plugins that don't need to do their own event dispatching.
 * If custom event dispatching is needed, execCommand shoul be overriden
 * instead.
 *
 * @param command The command to execute.
 * @param var_args Any additional parameters needed to
 *     execute the command.
 * @return The result of the execCommand, if any.
 *
  method execCommandInternal : js_string t -> ? -> ?*)

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the plugin handles this type of command.
 *)
  method isSupportedCommand : js_string t -> bool t meth

(**
 * Gets the state of this command if this plugin serves that command.
 * @param command The command to check.
 * @return The value of the command.
 *
  method queryCommandValue : js_string t -> *)
end


(**
   Plugin that adds support for table creation and editing commands.
*)
val tableEditor : tableEditor t constr
