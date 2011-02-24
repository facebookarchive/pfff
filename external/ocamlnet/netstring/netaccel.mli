(* $Id: netaccel.mli 800 2004-07-09 00:17:47Z stolpmann $ *)

(** Accelerators for bytecode
 *
 * This module can be linked with executables to accelerate
 * certain functions. In particular, the following functions
 * will run faster:
 *
 * - {!Netaux.ArrayAux.int_blit}
 * - All conversion functions in {!Netconversion} when they
 *   must read an ISO-8859-1 or UTF-8 encoded string
 *
 * It is not recommended to install the accelerators for native
 * code, however (and with the distributed build rules, this
 * is not done).
 *
 * To link this module, you must name both [netaccel.cma] and
 * [netaccel_link.cmo] explicitly on the ocamlc command line
 * (after [netstring.cma]).
 * If you use [findlib], this is done automatically.
 *)

(**/**)

val init : unit -> unit
