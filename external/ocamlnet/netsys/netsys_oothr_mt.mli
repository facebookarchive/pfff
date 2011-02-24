(* $Id$ *)

(** Object-oriented thread API *)

(** Makes the most important multi-threading primitives available using
    object types. These are the essential definitions.
 *)

open Netsys_oothr

exception Thread_val of Thread.t
exception Mutex_val of Mutex.t
exception Condition_val of Condition.t
  (** These exceptions are returned by the [repr] method of the 
      objects
   *)

val mtthread : Thread.t -> thread
  (** Create a thread object for a thread *)

val mtmutex : Mutex.t -> mutex
  (** Create a mutex object for a mutex *)

val mtcondition : Condition.t -> condition
  (** Create a condition object for a condition *)

val mtprovider : unit -> mtprovider
  (** Create a provider object *)

(** / **)

val init : unit -> unit
  (* Internal function. Called by netsys_oothr_mt_init *)
