(** 
   class goog.editor.plugins.ListTabHandler

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open AbstractTabHandler
#endif

class type listTabHandler = object
  inherit abstractTabHandler

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Handle a tab key press.
 * @param e The key event.
 * @return Whether this event was handled by this plugin.
 *)
  method handleTabKey : #Events.event t -> bool t meth
end

(**
   Plugin to handle tab keys in lists to indent and outdent.
*)
val listTabHandler : listTabHandler t constr
