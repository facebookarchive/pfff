(**
   class goog.editor.plugins.SpacesTabHandler

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open AbstractTabHandler
#endif

class type spacesTabHandler = object
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
   Plugin to handle tab keys when not in lists to add 4 spaces.
*)
val spacesTabHandler : spacesTabHandler t constr
