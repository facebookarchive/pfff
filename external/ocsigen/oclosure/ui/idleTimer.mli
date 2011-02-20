(*
   OClosure Project - 2010
   Class goog.ui.IdleTimer
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open ActivityMonitor
#endif

class type idleTimer = object
  inherit Events.eventTarget

(** @inheritDoc *)
  method disposeInternal : unit meth
    
(**
   @return the activity monitor keeping track of user
       interaction.
 *)
  method getActivityMonitor : activityMonitor t meth

(**
   @return the amount of time at which we consider the user has gone
       idle in ms.
 *)
  method getIdleThreshold : float t meth

(**
   Returns true if there has been no user action for at least the specified
   interval, and false otherwise
   @return true if the user is idle, false otherwise.
 *)
 method isIdle : bool t meth
end

(**
   Event target that will give notification of state changes between active and
   idle. This class is designed to require few resources while the user is
   active.
   @param idleThreshold Amount of time in ms at which we consider the
       user has gone idle.
   @param opt_activityMonitor The activity monitor
       keeping track of user interaction. Defaults to a default-constructed
       activity monitor. If a default activity monitor is used then this class
       will dispose of it. If an activity monitor is passed in then the caller
       remains responsible for disposing of it.
 *)
val idleTimer : float t -> activityMonitor t opt 
  -> idleTimer t constr
