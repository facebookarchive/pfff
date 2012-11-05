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

(** Tranformation of high level classes to low level classes. *)


(** Convert a high level class to a lower level class.*)
val high2low : JCode.jcode JClass.interface_or_class -> JClassLow.jclass



(** h2l_* functions are used in the JDump module so functions of
    JDumpLow can be used. *)

val h2l_cfield :
  JBasics.constant DynArray.t -> JClass.class_field -> JClassLow.jfield
val h2l_ifield :
  JBasics.constant DynArray.t -> JClass.interface_field -> JClassLow.jfield
val h2l_cmethod :
  JBasics.constant DynArray.t -> JCode.jcode JClass.concrete_method -> JClassLow.jmethod
val h2l_amethod :
  JBasics.constant DynArray.t -> JClass.abstract_method -> JClassLow.jmethod
val h2l_acmethod :
  JBasics.constant DynArray.t -> JCode.jcode JClass.jmethod -> JClassLow.jmethod
