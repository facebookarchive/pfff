(* $Id: netsys_signal.mli 1272 2009-10-01 01:33:31Z gerd $ *)

(** Signal handler framework

    This module defines a simple framework for setting signal handlers.
    When two modules want to set the handler for the same signal, the framework
    decides in which order the handlers are executed.

    The module also defines an empty handler list for [Sys.sigpipe], so these
    signals are ignored by the program. This empty list can be extended, 
    however.

    Win32: Only [Sys.sigint] handlers can effectively be registered. 
    Registrations for other signal types are accepted but ignored.
 *)

(* Win32: The Ocaml runtime makes a bad joke, and uses SIGTERM for
   interrupting the running program from the tick thread
 *)

val register_handler : 
      ?library:string ->
      ?priority:int ->
      ?keep_default:bool ->
      name: string ->
      signal: int ->
      callback:(int -> unit)->
      unit -> unit
  (** This function registers a handler called [name] for signal number
      [signal]. The handler function is [callback]. The int argument of
      the callback is the signal number.

      By default, the handler is an application handler. If [library] is set,
      the handler is for this library. The name passed as [library] is the
      findlib name of the library.

      By registering another handler for the same [library], [name], and
      [signal], the old handler is overridden.

      When several handlers are defined for the same signal, all handlers
      are executed that are defined for the signal (when the signal happens).
      The order of execution is given by [priority]. The handler functions
      are executed in ascending priority order. If the priority number is
      equal for two handlers, the order is undefined.

      The priority defaults to 0 for library handlers, and to 100 for
      application handlers. Libraries should only use values from 0 to 99,
      and applications only from 100 to 199.

      If all handlers for a certain signal set [keep_default], then there
      will be a special action after all signal handlers have been executed.
      The special action emulates the default behavior for the signal.
      For now, there is only a simple emulation: If the signal terminates
      the process, the process is immediately exited with code 126.
      If the default behaviour is "no-op", nothing happens. We don't try
      (yet) to do better (emulate core-dumps, emulate the right process
      status) because this is difficult in the general case.

      The handler definition takes place immediately.

      Any exceptions occuring during the execution of a handler are caught
      and ignored.
   *)

val register_exclusive_handler :
      name: string ->
      signal: int ->
      install:(unit -> unit)->
      unit -> unit
  (** An exclusive handler for a signal is the only handler for the signal.
      If it is tried to register another handler when there is already
      an exclusive handler, the second registration fails. Also, an
      exclusive handler cannot be registered when there is already a normal
      handler for the signal. It is, however, possible to replace the
      registered exclusive handler by another exclusive handler for the
      same signal.

      An exclusive handler is installed by running the [install] function,
      which can e.g. call [Sys.set_signal] to define the handler. Other
      methods (e.g. use some C wrapper) are also possible. It is assumed
      that [install] overrides any existing handler.
   *)

val restore_management : int -> unit
  (** [restore_management signo]: Restores signal handling management for
      [signo] as defined by
      the handler list for this signal. Calling [restore_management] makes
      sense when
      the signal handler has been overridden with [Sys.set_signal], but at
      some point this module is again responsible for managing the signal
      handling for this signal.
   *)
      

val keep_away_from : int -> unit
  (** [keep_away_from signo]: This signal [signo] is added to the 
      "keep away list". This means that this module will never try to 
      change the signal behavior again for [signo]. Even [restore_management]
      will not restore the signal handling again. This function should only
      by called by applications wishing to do the signal handling all 
      themselves.

      This function does not have any effect on the already installed
      handlers. It is nevertheless useful for applications calling
      [Sys.set_signal] directly to ensure that [Netsys_signal] will never
      again try to override the handler.
   *)

type action =
    [ `Callback of int -> unit
    | `Install of unit -> unit
    ]
  (** [`Callback] is used for normal handlers, and [`Install] for exclusive
      handlers.
   *)

type entry =
    { sig_number : int;
      sig_library : string option;
      sig_priority : int;
      sig_keep_default : bool;
      sig_name : string;
      sig_action : action;
    }

val list : unit -> entry list
  (** Returns the list of signal handlers *)

val keep_away_list : unit -> int list
  (** Returns the "keep away list". *)

val init : unit -> unit
  (** Call this function to ensure that this module is initialized. It is
      also possible to call any other function. After initialization the
      Sigpipe handler is set.
   *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module  *)
end
