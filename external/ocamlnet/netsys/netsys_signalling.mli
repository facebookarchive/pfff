(* $Id: netsys_signalling.mli 1160 2007-12-03 00:10:15Z gerd $ *)

(** Signalling between threads in a poll-compatible way *)

(** We define here an abstraction that can be both used on Unix and
    Win32 to signal special conditions between threads. It is
    a kind of semaphore tied to file descriptors.
 *)

class type sigchannel =
object
  method file_descr : Unix.file_descr
    (** A file descriptor that can be used for polling until the signal
        arrives. One has to poll for reading.
     *)

  method receive : unit -> unit
    (** Receive a signal. If no signal is available, the function will block
        until a signal arrives. One receives as many signals as have been
        sent.
     *)

  method send : unit -> unit
    (** Send a signal. Never blocks. *)

  method dispose : unit -> unit
    (** Release OS resources associated with this channel. Implicitly
        closes [file_descr].
     *)

end


val create_sigchannel : unit -> sigchannel
  (** Creates a new signal channel - for all platforms.

      Unix: Signalling is implemented using a pipe. By sending a single
      over the pipe, it is communicated that a positive number of signals
      is available for receiving. The file desciptor can be used with
      any form of polling (any Unix pollset will do).

      Win32: The file descriptor is in reality a Win32 event object.
      When a positive number of signals is available for receiving,
      the event object is set. The file descriptor can be used with
      [Netsys_pollset_win32.sigchannel_pollset].

      Note that signal channels are also avaiable for single-threaded
      programs, although they are probably not that useful there.
   *)
