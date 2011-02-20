(*
   OClosure Project - 2010
   Class goog.ui.ActivityMonitor
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
#endif

class type activityMonitor = object
  inherit Events.eventTarget

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   Returns the amount of time the user has been idle.
   @param opt_now The current time can optionally be passed in for the
   computation to avoid an extra Date allocation.
   @return The amount of time in ms that the user has been idle.
*)
  method getIdleTime : float opt -> float t meth

(**
   Returns the time of the last event
   @return last event time.
*)
  method getLastEventTime : float t meth

(**
   Returns the type of the last user event.
   @return event type.
*)
  method getLastEventType : js_string t meth

(**
   Updates the last event time to be the present time, useful for non-DOM
   events that should update idle time.
*)
  method resetTimer : unit meth
end

(**
  Once initialized with a document, the activity monitor can be queried for
   the current idle time.
   TODO(user): Expand this class to allow it to monitor multiple DOMs.
   
   @param opt_domHelper
   DomHelper which contains the document(s) to listen to.  If null, the
   default document is usedinstead.
*)
val activityMonitor : 
    (Gdom.domHelper t, Gdom.domHelper t js_array t) Tools.Union.t 
    -> activityMonitor t constr

