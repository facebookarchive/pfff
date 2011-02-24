(* $Id: netsys_pollset_win32.mli 1262 2009-08-31 18:14:21Z gerd $ *)

(** Pollsets for Win32 *)

open Netsys_pollset

exception Too_many_descriptors

val pollset : unit -> pollset
  (** This is a pollset implementation that works for 
       - sockets, and
       - named pipes as provided by {!Netsys_win32} (add the descriptors
         returned by [pipe_descr] or [pipe_server_descr] to the pollset)

      The number of descriptors that can be added to the pollset
      has a quite low limit (usually 63 sockets or 31 pipes).
      If the number is exceeded the exception [Too_many_descriptors]
      is raised (by [add]).

      POLLERR, POLLHUP, and POLLNVAL are not detected by this impl.
   *)

val threaded_pollset : unit -> pollset
  (** This implementation overcomes the limit on the number of descriptors 
      one can add
      to the set. It is, however, only available for multi-threaded
      programs, because it uses helper threads.
   *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module  *)
end
