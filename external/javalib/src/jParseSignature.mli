(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
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

(** This module handles the decoding of descriptors and signatures.*)

open JBasics
open JSignature

(** {2 Parsing descriptors and their components } *)

(** @raise Class_structure_error if the string does not correspond the an object
    type.*)
val parse_objectType : string -> object_type

(** @raise Class_structure_error if the string does not correspond the a field
    descriptor.*)
val parse_field_descriptor : string -> value_type

(** @raise Class_structure_error if the string does not correspond the a method
    descriptor.*)
val parse_method_descriptor : string -> value_type list * value_type option

(** @raise Class_structure_error if the string does not correspond the a
    descriptor.*)
val parse_descriptor : string -> descriptor



(** {2 Parsing generic signatures} *)

(** [parse_ClassSignature s] parses a Signature attribute and expects
    to find a ClassSignature (as describe in paragraph 4.4.4 of the
    Java Virtual Machine Specification of Java 5).

    @raise Class_structure_error if the signature does not correspond
    to the specifications. *)
val parse_ClassSignature : string -> classSignature

(** [parse_MethodTypeSignature s] parses a Signature attribute and
    expects to find a Methodtypesignature (as describe in paragraph
    4.4.4 of the Java Virtual Machine Specification of Java 5).

    @raise Class_structure_error if the signature does not correspond
    to the specifications. *)
val parse_MethodTypeSignature : string -> methodTypeSignature

(** [parse_FieldTypeSignature s] parses a Signature attribute [s] and
    expects to find a FieldTypeSignature (as describe in paragraph
    4.4.4 of the Java Virtual Machine Specification of Java 5).

    @raise Class_structure_error if the signature does not correspond
    to the specifications. *)
val parse_FieldTypeSignature : string -> fieldTypeSignature
