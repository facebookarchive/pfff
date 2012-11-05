(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007 Laurent Hubert (CNRS)
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

(** Unparsing (compilation) of either high or low level class
    representations into class files. *)

(** Provided constants are kept unchanged. Missing constant are added at
    the end of the constant pool if needed. *)

(** Unparses a class to a file. See {!JInstruction.code2opcodes} for more
    information.

    @raise JParseCode.OpcodeLengthError if an opcode cannot be encoded in the
    allocated place. *)
val unparse_class_low_level : 'a IO.output -> JClassLow.jclass -> unit

(** Unparses a class to a file. See {!JInstruction.code2opcodes} for more
    information.

    @raise JParseCode.OpcodeLengthError if an opcode cannot be encoded in the
    allocated place. *)
val unparse_class : 'a IO.output -> JCode.jcode JClass.interface_or_class -> unit

(** Unparses an attribute to a couple [(name, content)] where [name]
    is the code-name of the attribute and [content] as encoded in a
    [.class] file. *)
val unparse_attribute_to_strings : JBasics.constant DynArray.t -> JClassLow.attribute -> string * string

(**/**)

(* For statistics: *)

val unparse_stackmap_attribute :
  JBasics.constant DynArray.t ->
  (int * JBasics.verification_type list * JBasics.verification_type list) list ->
  (string * string)

val unparse_stackmap_table_attribute :
  JBasics.constant DynArray.t -> JClassLow.stackmap_frame list -> (string * string)

val unparse_constant_pool :
  'a IO.output ->
  JBasics.constant DynArray.t ->
  unit
