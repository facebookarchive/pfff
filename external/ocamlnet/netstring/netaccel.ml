(* $Id: netaccel.ml 798 2004-07-08 22:11:07Z stolpmann $ *)

external int_blit
  : int array -> int -> int array -> int -> int -> unit
  = "netstring_int_blit_ml" ;;

external int_series
  : int array -> int -> int array -> int -> int -> int -> unit
  = "netstring_int_series_byte"  "netstring_int_series_ml";;


external read_iso88591 
  : int -> Netconversion.encoding -> int array -> int array -> string -> int -> int -> 
    (int*int*Netconversion.encoding)
  = "netstring_read_iso88591_byte" "netstring_read_iso88591_ml" ;;

external read_utf8
  : bool -> int array -> int array -> string -> int -> int -> 
    (int*int*Netconversion.encoding)
  = "netstring_read_utf8_byte" "netstring_read_utf8_ml" ;;

let init() =
  Netaux.ArrayAux.int_blit_ref := int_blit;
  Netaux.ArrayAux.int_series_ref := int_series;
  Netconversion.read_iso88591_ref := read_iso88591;
  Netconversion.read_utf8_ref := read_utf8;;
