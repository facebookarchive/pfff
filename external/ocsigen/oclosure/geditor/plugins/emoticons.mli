(**
   class goog.editor.plugins.Emoticons

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type emoticons = object 
  inherit plugin

(**
 * Inserts an emoticon into the editor at the cursor location. Places the
 * cursor to the right of the inserted emoticon.
 * @param command Command to execute.
 * @param emoji Emoji to insert.
 * @return The result of the command.
 *)	
(*  method execCommandInternal : js_string t -> ? -> ? *)

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
end

/**
 * Plugin for generating emoticons.
 *
 * @constructor
 */
val emoticons : emoticons t constr 
