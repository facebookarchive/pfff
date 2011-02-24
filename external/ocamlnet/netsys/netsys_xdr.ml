(* $Id: netsys_xdr.ml 1355 2009-12-10 21:39:01Z gerd $ *)

external s_read_int4_64_unsafe : string -> int -> int
  = "netsys_s_read_int4_64" "noalloc"

external s_write_int4_64_unsafe : string -> int -> int -> unit
  = "netsys_s_write_int4_64" "noalloc"

external s_read_string_array_unsafe : 
  string -> int -> int -> int32 -> string array -> int
  = "netsys_s_read_string_array"
