(* $Id: netsys_xdr.mli 1355 2009-12-10 21:39:01Z gerd $ *)

(** Some helpers for en/decoding XDR faster *)

external s_read_int4_64_unsafe : string -> int -> int
  = "netsys_s_read_int4_64" "noalloc"
  (** For 64 bit platforms only: Decodes 4 bytes at this string position
      as a signed 32 bit int in network byte order

      There is no index bounds check!
   *)

external s_write_int4_64_unsafe : string -> int -> int -> unit
  = "netsys_s_write_int4_64" "noalloc"
  (** For 64 bit platforms only: Encodes 4 bytes at this string position
      as a signed 32 bit int in network byte order

      There is no index bounds check!
   *)

external s_read_string_array_unsafe : 
  string -> int -> int -> int32 -> string array -> int
  = "netsys_s_read_string_array"
  (** [let pos' = s_read_string_array s pos len max a]: 
      Decodes the XDR repr of an array of strings {b without} length field.
      The array must start at [pos] in [s], and may have up to [len] bytes.
      The array is passed in [a], and the elements are set by this routine.
      Returns in [pos'] the position of the first
      byte after the array. It is checked whether
      the array is represented within the allowed [len] bytes. If not,
      [pos'=-1] will be returned, together with an empty array.

      [max] is the maximum length of the string elements. [max] is
      unsigned. If this is violated, [pos'=-2] will be returned.
   *)
