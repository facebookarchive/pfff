(**
   class goog.editor.plugins.LoremIpsum

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type loremIpsum = object 
  inherit plugin

(**
 * @return If true, field will not disable the command
 *     when the field becomes uneditable.
 *)
  method activeOnUneditableFields : bool t meth

(**
 * Handles execCommand.
 * @param command The command to execute.
 *     Should be CLEAR_LOREM or UPDATE_LOREM.
 * @param placeCursor Whether to place the cursor in the field
 *     after clearing lorem.
 *)
  method execCommand : js_string t -> bool t -> unit meth

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
 * Handles queryCommandValue.
 * @param command The command to query.
 * @return The result.
 *)
  method queryCommandValue : js_string t -> bool t meth
end

(**
 * A plugin that manages lorem ipsum state of editable fields.
 * @param message The lorem ipsum message.
 *)
val loremIpsum : (js_string t -> loremIpsum t) constr
