(* $Id: netsys_pollset_posix.mli 1202 2008-09-04 22:11:24Z gerd $ *)

(** Pollsets for POSIX operating systems *)

open Netsys_pollset

val poll_based_pollset : unit -> pollset
  (** Returns a poll set whose implementation is based on the [poll] system
      call. 

      Win32: On Win32 this implementation works, but only for sockets,
      and is not cancellable in multi-threaded programs. (This is a 
      restriction because we have to map it to the [select] call of the
      WinSock layer.)
   *)

val reset : unit -> unit
  (** This module may keep some global state. This function resets this
      state. As the state may contain file descriptors, it is advisable
      to reset after calling [fork] to free these descriptors.
   *)


(* TODO: pollsets for epoll, kqueue, /dev/poll etc. *)
