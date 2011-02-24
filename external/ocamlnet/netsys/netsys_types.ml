(* WARNING! THIS IS A COPY OF NETSYS_TYPES.MLI! *)
(* $Id: netsys_types.mli 1387 2009-12-31 15:22:46Z gerd $ *)

(** Types for all Netsys modules *)

type memory = 
    (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t
  (** We consider 1-dimensional bigarrays of chars as memory buffers.
      They have the useful property that the garbage collector cannot
      relocate them, i.e. the address is fixed. Also, one can mmap
      a file, and connect the bigarray with shared memory.
   *)
