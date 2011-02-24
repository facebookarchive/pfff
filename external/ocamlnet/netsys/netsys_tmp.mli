(* $Id: netsys_tmp.mli 1518 2010-12-19 19:33:45Z gerd $ *)

(** Temporary files *)

val tmp_directory : unit -> string
  (** Return the directory for temporary files. This is used as global
      default for functions that need such a directory. It is initialized
      with a reasonable default (OS-dependent).
   *)

val tmp_prefix : string -> string
  (** [tmp_prefix p]: Enhance the prefix for temporary files by appending
      some digits to [p]. It is not ensures that the prefix is unique,
      though.
   *)

val set_tmp_directory : string -> unit
  (** Set the directory for temporary files *)
