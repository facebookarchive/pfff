(* $Id: netdb.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(* This is an internal interface of ocamlnet! Do not use outside! *)

(* This module manages persistent values (often lookup tables). These
 * values can be stored in external files, or they can be initialized
 * from string values.
 *)

val read_db : string -> 'a
  (* Reads the value with the given name, and returns it. This function
   * does not cache the returned value, and the marshalled string is
   * deserialized every time the function is called.
   *
   * First it is checked whether there was a set_db call, and if so,
   * this value is unmarshalled and returned. Otherwise, it is checked
   * whether there is a matching external file (unless file lookup is 
   * disabled), and if so, the file is loaded and unmarshalled.
   * If neither of the two methods works, the function fails.
   *)

val exists_db : string -> bool
  (* Checks whether the named value is available, i.e. read_db would
   * be able to find it
   *)

val set_db : string -> string -> unit
  (* Sets the persistent value with the given name (1st arg) to the 
   * passed value (2nd arg). The value must be marshalled as string.
   *)


val disable_file_db : unit -> unit
  (* Disables file lookup. *)

