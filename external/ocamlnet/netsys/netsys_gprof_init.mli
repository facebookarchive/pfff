(* $Id: netsys_gprof_init.mli 1401 2010-02-03 23:25:36Z gerd $ *)

(** Initialize GPROF helper

    By linking this module the function {!Netsys.moncontrol} is made
    working. This should only be done if the program is built for
    profiling (ocamlopt -p).
 *)

val init : unit -> unit
  (** Dummy function. *)
