(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gpointer.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

(** [Gpointer]: various kinds of pointers to C data *)

(** Marked pointers *)
type 'a optaddr
val optaddr : 'a option -> 'a optaddr

(** Naked pointers *)
type optstring
val raw_null : optstring
val optstring : string option -> optstring

(** Boxed pointers *)
type boxed
val boxed_null : boxed
val peek_string : ?pos:int -> ?len:int -> boxed -> string
val peek_int : boxed -> int
val poke_int : boxed -> int -> unit
val peek_nativeint : boxed -> nativeint
val poke_nativeint : boxed -> nativeint -> unit

type 'a optboxed
val optboxed : 'a option -> 'a optboxed
val may_box : f:('a -> 'b) -> 'a option -> 'b optboxed

(** Variant tables *)
type 'a variant_table constraint 'a = [> ]
val decode_variant : 'a variant_table -> int -> 'a
val encode_variant : 'a variant_table -> 'a -> int
val decode_flags : 'a variant_table -> int -> 'a list
val encode_flags : 'a variant_table -> 'a list -> int

(** Null pointer exception *)
exception Null

(** Ensure a value is copied in the old generation *)
type 'a stable
val stable_copy : 'a -> 'a stable

(** Region handling *)

(** The abstract type of heap regions *)
type region

val length : region -> int
(** the length of the region *)
val get_addr : region -> nativeint
(** the start address of the region *)

val sub : ?pos:int -> ?len:int -> region -> region
(** subregion of length [len] starting at offset [pos] *)

val get_byte : region -> pos:int -> int
val set_byte : region -> pos:int -> int -> unit
val blit : src:region -> dst:region -> unit

val region_of_string : string -> region
(** create a region sharing a string *)
val string_of_region : region -> string
(** copy the contents of the region to a string *)

type 'a bigarray = (int, Bigarray.int8_unsigned_elt, 'a) Bigarray.Array1.t
val region_of_bigarray : 'a bigarray -> region
(** create a region sharing a bigarray *)

(** Unsafe access *)

val unsafe_create_region :
  path:int array -> get_length:('a -> int) -> 'a -> region
(** [unsafe_create_region ~path ~get_length] returns a function
    to build regions from a specific kind of data abstraction *)

external unsafe_get_byte : region -> pos:int -> int = "ml_gpointer_get_char"
external unsafe_set_byte : region -> pos:int -> int -> unit
  = "ml_gpointer_set_char"
