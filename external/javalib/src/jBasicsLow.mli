(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

(** Low level manipulations on basic types. *)

open JBasics

(** {2 Constant Pool.}  *)

(** Getting a constant from the constant pool: *)

val get_constant : constant array -> int -> constant
val get_constant_value : constant array -> int -> constant_value
val get_object_type : constant array -> int -> object_type
val get_class : constant array -> int -> class_name
val get_string : constant array -> int -> string
val get_field : constant array -> int ->
  class_name * field_signature
val get_method : constant array -> int ->
  object_type * method_signature
val get_interface_method : constant array -> int ->
  class_name * method_signature

(** Same thing, reading the index in a channel: *)

val get_class_ui16 : constant array -> IO.input -> class_name
val get_string_ui16 : constant array -> IO.input -> string

(** Getting an index for a constant: *)

(** Return the index of a constant, adding it to the constant pool if necessary. *)
val constant_to_int : constant DynArray.t -> constant -> int
val value_to_int : constant DynArray.t -> constant_value -> int
val object_type_to_int : constant DynArray.t -> object_type -> int
val class_to_int : constant DynArray.t -> class_name -> int
val field_to_int : constant DynArray.t ->
  class_name * field_signature -> int
val method_to_int : constant DynArray.t ->
  object_type * method_signature -> int
val string_to_int : constant DynArray.t -> string -> int

(** Same thing, but writes the index to a channel. *)

val write_constant :
  'a IO.output -> constant DynArray.t -> constant -> unit
val write_value :
  'a IO.output -> constant DynArray.t -> constant_value -> unit
val write_object_type :
  'a IO.output -> constant DynArray.t -> object_type -> unit
val write_class :
  'a IO.output -> constant DynArray.t -> class_name -> unit
val write_string :
  'a IO.output -> constant DynArray.t -> string -> unit
val write_name_and_type :
  'a IO.output -> constant DynArray.t -> string * descriptor -> unit

(** {2 Usefull writing functions. } *)

(** @raise Overflow if the integer does not belong to [0x0;0xFF].  *)
val write_ui8 : 'a IO.output -> int -> unit

(** @raise Overflow if the integer does not belong to [-0x80;0x7F].  *)
val write_i8 : 'a IO.output -> int -> unit

val write_string_with_length :
  ('a IO.output -> int -> unit) -> 'a IO.output -> string -> unit
val write_with_length :
  ('a IO.output -> int -> unit) ->
  'a IO.output -> (string IO.output -> unit) -> unit
val write_with_size :
  ('a IO.output -> int -> unit) -> 'a IO.output -> ('c -> unit) -> 'c list -> unit

