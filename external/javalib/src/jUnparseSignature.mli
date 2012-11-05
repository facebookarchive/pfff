(*
 * This file is part of Javalib
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

(** This module "unparses" (converts back to strings) descriptors and
    signatures. *)

open JBasics

val unparse_value_type : value_type -> string
val unparse_object_type : object_type -> string
val unparse_method_descriptor : value_type list * value_type option -> string
val unparse_descriptor : descriptor -> string
val unparse_constClass : object_type -> string

open JSignature

val unparse_ClassSignature : classSignature -> string
val unparse_FieldTypeSignature : fieldTypeSignature -> string
val unparse_MethodTypeSignature : methodTypeSignature -> string
