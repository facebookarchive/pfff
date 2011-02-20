(**
   class goog.editor.plugins.Blockquote

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type blockquote = object
  inherit plugin

(**
 * Splits a quoted region if any.  To be called on a key press event.  When this
 * function returns true, the event that caused it to be called should be
 * canceled.
 * @param command The command to execute.
 * @param var_args Single additional argument representing the
 *     current cursor position.  In IE, it is a single node.  In any other
 *     browser, it is an object with a [node] key and an [offset]
 *     key.
 * @return Boolean true when the quoted region has been
 *     split, false or undefined otherwise.
 * @override
 *)
(*  method execCommandInternal : js_string t -> ? -> bool t optdef meth *)
  
(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Whether the given node is an already set up blockquote.
 * @param node DOM node in question.
 * @return Whether this node is an already setup blockquote.
 *)
  method isSetupBlockquote : #Dom.node t -> bool t meth

(**
 * Since our exec command is always called from elsewhere, we make it silent.
 *)
  method isSilentCommand : js_string t -> bool t meth

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the plugin handles this type of command.
 *)
  method isSupportedCommand : js_string t -> bool t meth
end

(**
 * Plugin to handle splitting block quotes.  This plugin does nothing on its
 * own and should be used in conjunction with EnterHandler or one of its
 * subclasses.
 * @param requiresClassNameToSplit Whether to split only blockquotes
 *     that have the given classname.
 * @param opt_className The classname to apply to generated
 *     blockquotes.  Defaults to 'tr_bq'.
 *)
val blockquote : (bool t -> js_string t opt -> blockquote t) constr
