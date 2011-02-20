open Js
open Tools

#define EVENTS
#include "event.mli"
#include "eventType.mli"
#include "browserEvent.mli"
#include "eventTarget.mli"
#include "eventHandler.mli"
#include "keyEvent.mli"
#include "keyHandler.mli"

(**
   Adds an event listener for a specific event on a DOM Node or an object that 
   has implemented goog.events.EventTarget. A listener can only be added once to
   an object and if it is added again the key for the listener is returned. 

   @param src The node to listen to events on
   @param type Event type
   @param listener Callback method, or an object with a handleEvent function
   @param opt_capt Whether to fire in capture phase (defaults to false)
   @return Unique key for the listener.*)
val listen : (#eventTarget t, #Dom_html.eventTarget t) Union.t -> js_string t 
  -> (unit -> unit) callback -> bool t opt -> int
