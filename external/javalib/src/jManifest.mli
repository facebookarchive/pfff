(*
 * This file is part of Javalib
 * Copyright (c)2010 Tiphaine Turpin (Université de Rennes 1)
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
(** Accessing meta-information in jar files. *)

(** The type of manifest files: *)

type main_section = {
  manifest_version : int list;
  main_attributes : (string * string) list;
}

type section = {
  name : string;
  attributes : (string * string) list;
}

type manifest = {
  main_section : main_section;
  individual_sections : section list;
}

(** Read a manifest from a jar file (which must end with .jar). *)
val jar2manifest : string -> manifest

(** Get a midlet's main class from the MIDlet-1 attribute of its manifest. *)
val midlet_main_class : manifest -> string

(** Other functions *)

(** Read a list of sections. This function may be used for reading
    various files from the META-INF directory of a jar file, including
    the MANIFEST.MF file *)
val sections : Lexing.lexbuf -> (string * string) list list

(** Interpret a list of section as a manifest. *)
val sections2manifest : (string * string) list list -> manifest
