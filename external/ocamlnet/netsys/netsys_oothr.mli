(* $Id: netsys_oothr.mli 1247 2009-05-27 19:52:09Z gerd $ *)

(** Object-oriented thread API *)

(** Makes the most important multi-threading primitives available using
    object types. For single-threaded apps, the operations are substituted
    by no-ops.
 *)

class type mtprovider =
object
  method single_threaded : bool
    (** Whether this is a single-threaded program. In this case, a number
        of methods change their meaning, as described below.
     *)

  method create_thread : 's 't . ('s -> 't) -> 's -> thread
    (** In a multi-threaded program: Starts a new thread, and calls the
        passed function with the passed argument in the new thread
        (like [Thread.create]).
        In a single-threaded program: fails.
     *)

  method self : thread
    (** In a multi-threaded program: Returns the currently running thread.
        Subsequent calls of [self] can return different objects for the
        same thread, but the [id] method will always return the same number.
        In a single-threaded program: Returns a dummy object (see below).
     *)

  method yield : unit -> unit
    (** In a multi-threaded program: gives a hint that another thread should
        better run now.
        In a single-threaded program: this is a no-op.
     *)

  method create_mutex : unit -> mutex
    (** In a multi-threaded program: Creates a mutex and returns the object.
        In a single-threaded program: Returns a dummy mutex object (see below).
     *)

  method create_condition : unit -> condition
    (** In a multi-threaded program: Creates a condition variable and returns 
        the object.
        In a single-threaded program: Returns a dummy variable (see below).
     *)
end

and thread =
object
  method id : int
    (** In a multi-threaded program: Returns the thread ID.
        In a single-threaded program: Returns 0.
     *)

  method join : unit -> unit
    (** In a multi-threaded program: Suspends the calling thread until this
        thread terminates.
        In a single-threaded program: fails
     *)

  method repr : exn
    (** May be used internally be the implementation *)
end

and mutex =
object
  method lock : unit -> unit
    (** In a multi-threaded program: Suspends the calling thread until the
        mutex is locked.
        In a single-threaded program: This is a no-op
     *)

  method unlock : unit -> unit
    (** In a multi-threaded program: Unlocks a mutex.
        In a single-threaded program: This is a no-op
     *)

  method try_lock : unit -> bool
    (** In a multi-threaded program: Tries to immediately lock the mutex,
        and returns whether the lock could be obtained.
        In a single-threaded program: returns [true]
     *)

  method repr : exn
    (** May be used internally be the implementation *)

end

and condition =
object
  method wait : mutex -> unit
    (** In a multi-threaded program: Wait for the condition to be true
        and use the mutex to protect this situation.
        In a single-threaded program: this is a no-op.
     *)

  method signal : unit -> unit
    (** In a multi-threaded program: Signal one process that the condition
        holds.
        In a single-threaded program: this is a no-op.
     *)


  method broadcast : unit -> unit
    (** In a multi-threaded program: Signal all waiting processes that the
        condition holds.
        In a single-threaded program: this is a no-op.
     *)

  method repr : exn
    (** May be used internally be the implementation *)

end

val provider : mtprovider ref
  (** Return the multi-threading provider *)

val serialize : mutex -> ('a -> 'b) -> 'a -> 'b
  (** [serialize m f arg]: Locks [m], runs [f arg], unlocks [m], and returns
      the result.
   *)
