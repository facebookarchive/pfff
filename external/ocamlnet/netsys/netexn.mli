(* $Id: netexn.mli 1266 2009-09-17 13:50:26Z gerd $ *)

(** Exception registry

    This module allow the user to register exceptions, and to set a
    custom exception printing function. This results in nicer exception
    prints when [Netexn.to_string] is called instead of [Printexc.to_string].

    Generally, [Printexc.to_string] works well if:
     - the exception does not have arguments, or
     - all arguments are int's or string's.

    In these cases it is not required to register a custom printer. Arguments
    that are neither int nor string are printed as "_", however, so defining
    a custom printer helps then.

    The printers registered here are intended for helping debugging, so
    the goal is to print all of the exception in an unambiguous way.
    The syntax "Exception(arg1, arg2, ...)" is preferred.

    {b Since OCaml 3.11.2 there is a similar feature in [Printexc].}
    If Ocamlnet recognizes that [Printexc.register_printer] is available,
    all functions registered in this module are also registered at
    the central [Printexc] registry. For users it does not make a difference
    then whether [Netexn.to_string] or [Printexc.to_string] is called
    to print the exception. We recommend, however, to use [Netexn] as
    primary registry for all code that uses Ocamlnet because this
    mechanism also works for older Ocaml versions, and is slightly more
    efficient.

 *)

val register_printer : exn -> (exn -> string) -> unit
  (** [register e f]: Register that the exception type of the sample
      exception [e] is to be printed by [f].

      In multi-threaded programs, this function should no longer be called
      once threads have been spawned.
   *)

val to_string : exn -> string
  (** Prints the exception as string, using the registered printer functions,
      or [Printexc.to_string] as fallback if there is no better printer
   *)
