(*
   OClosure Project - 2010
   Class goog.ui.MockActivityMonitor
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open ActivityMonitor
#endif

class type mockActivityMonitor = object
  inherit activityMonitor

(**
   Simulates an event that updates the user to being non-idle.
   @param opt_type The type of event that made the user
   not idle. If not specified, defaults to MOUSEMOVE.
*)
  method simulateEvent : Events.eventType t opt -> unit meth
end

(**
   A mock implementation of goog.ui.ActivityMonitor for unit testing. Clients
   of this class should override goog.now to return a synthetic time from
   the unit test.
*)
val mockActivityMonitor : mockActivityMonitor t constr

