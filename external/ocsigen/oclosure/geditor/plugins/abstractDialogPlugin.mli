(**
   class goog.editor.plugins.AbstractDialogPlugin

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif
#ifndef EDITOR
open PluginField
#endif

class type abstractDialogPlugin = object
  inherit plugin

(** @inheritDoc *) 
  method disposeInternal : unit meth

(**
 * Handles execCommand. Dialog plugins don't make any changes when they open a
 * dialog, just when the dialog closes (because only modal dialogs are
 * supported). Hence this method does not dispatch the change events that the
 * superclass method does.
 * @param command The command to execute.
 * @param var_args Any additional parameters needed to
 *     execute the command.
 * @return The result of the execCommand, if any.
 * @override
 *)
  (* method execCommand *)

(** @inheritDoc *)
  method isSupportedCommand : js_string t -> bool t meth
end
