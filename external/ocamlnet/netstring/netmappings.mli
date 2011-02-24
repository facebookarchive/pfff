(* $Id: netmappings.mli 1219 2009-04-14 13:28:56Z ChriS $
 * ----------------------------------------------------------------------
 *)

(** Internal access to the character conversion database
 *
 * This is an internal module.
 *)

type from_uni_list =
    U_nil
  | U_single of (int*int)
  | U_double of (int*int * int*int)
  | U_array of int array
;;
  (* A representation of (int*int) list that is optimized for the case that
   * lists with 0 and 1 and 2 elements are the most frequent cases.
   *)


val get_to_unicode : string -> int array 

val get_from_unicode : string -> from_uni_list array
  (* These functions get the conversion tables from local encodings to
   * Unicode and vice versa.
   * It is normally not necessary to access these tables; the 
   * Netconversion module does it already for you.
   *
   * The argument is the internal name of the encoding. (E.g. if 
   * encoding = `Enc_iso88591, the internal name is "iso88591", i.e.
   * the "`Enc_" prefix is removed. However, for "composite encodings"
   * like `Enc_eucjp things are more complicated.)
   *
   * Specification of the conversion tables:
   *
   * to_unicode: maps a local code to Unicode, i.e.
   *    let m = Hashtbl.find `Enc_isoXXX to_unicode in
   *    let unicode = m.(isocode) 
   *    - This may be (-1) to indicate that the code point is not defined.
   *
   * from_unicode: maps Unicode to a local code, i.e.
   *    let m = Hashtbl.find `Enc_isoXXX from_unicode in
   *    let l = m.(unicode land mask)
   *    Now search in l the pair (unicode, isocode), and return isocode.
   *    Where mask = Array.length from_unicode - 1
   *)

val lock : unit -> unit
  (* In multi-threaded applications: obtains a lock which is required to
   * Lazy.force the values found in to_unicode and from_unicode.
   * In single-threaded applications: a NO-OP
   *)

val unlock : unit -> unit
  (* In multi-threaded applications: releases the lock which is required to
   * Lazy.force the values found in to_unicode and from_unicode.
   * In single-threaded applications: a NO-OP
   *)
