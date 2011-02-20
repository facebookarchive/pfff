(*
   OClosure Project - 2010
   Class goog.ui.KeyboardShortcutEvent

   @author Cardoso Gabriel
   @version 0.2
 *)
#ifndef UI
open Js
#endif
class type keyboardShortcutEvent = object
  inherit Events.event
end

(**
   Object representing a keyboard shortcut event.
   @param type Event type.
   @param identifier Task identifier for the triggered shortcut.
   @param target Target the original key press
   event originated from.
*)
val keyboardShortcutEvent : (js_string t -> js_string t -> 
  (#Dom.node t, #Events.eventTarget t) Tools.Union.t opt 
  -> keyboardShortcutEvent t) constr
