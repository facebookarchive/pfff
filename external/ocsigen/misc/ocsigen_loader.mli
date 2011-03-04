(* Ocsigen
 * http://www.ocsigen.org
 * File ocsigen_loader.mli
 * Copyright (C) 2008 Stéphane Glondu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Module [Ocsigen_loader]: Dynamic loading for Ocsigen. *)

(** Notes about Findlib usage:
    - Findlib is called with predicates "plugin", "mt". Moreover, the
      predicate "native" or "byte" is added, depending on whether
      Ocsigen is running in native or bytecode mode.
    - In native mode, .cmx/.cmxa extensions provided by META files are
      replaced by .cmxs.
    - The OCAMLPATH environment variable is ignored altogether.
*)

exception Dynlink_error of string * exn
exception Findlib_error of string * exn

val translate : string -> string
  (** [translate filename] translate .cmo/.cma extensions to .cmxs in
      native mode, and .cmxs to .cmo (.cma if it exists) in bytecode
      mode. *)

val set_init_on_load: bool -> unit
  (** If set to [true], the module initialization functions passed to
      [set_module_init_function] will be executed directly. Otherwise,
      they will have to be invoked using [init_module] at some later stage. *)

val loadfile: (unit -> unit) -> (unit -> unit) -> bool -> string -> unit
  (** [loadfile pre post force file] (dynamically) loads [file]. If
      [force] is [false], remember [file] so that it isn't loaded
      twice. If the loading effectively occurs, [pre] (resp. [post])
      is called before (resp. after) the loading. [post] will be
      called even if the loading fails. *)

val loadfiles: (unit -> unit) -> (unit -> unit) -> bool -> string list -> unit
  (** [loadfiles pre post force file] loads all the [files], using
      [loadfile (fun () -> ()) (fun () -> ()) false] for all the files
      but the last one, and [loadfile pre post force] for the last one
      (if any). *)

val set_module_init_function : string -> (unit -> unit) -> unit
  (** [set_module_init_function name f] registers the function [f], which will
      be used to initialize the module when [init_module name] is called.  *)

val init_module : (unit -> unit) -> (unit -> unit) -> bool -> string -> unit
  (** [init_module pre post force name] runs the init function for the module
      [name]. If [force] is [false], remember [name] so that the init function
      isn't executed twice. If the function is executed, [pre] (resp. [post])
      is called before (resp. after) the loading. [post] will be
      called even if the loading fails. *)

val get_ocamlpath: unit -> string list
  (** Returns the current Findlib library search path. *)

val set_ocamlpath: string list -> unit
  (** Sets the current Findlib library search path. The OCaml standard
      library path and some site-specific paths are always implicitly
      added. *)

val add_ocamlpath: string -> unit
  (** Adds a path to the Findlib library search path. *)

val findfiles: string -> string list
  (** [findfiles pkg] returns the list of files needed to load Findlib
      package [pkg], including dependencies. The archive files of
      [pkg] will appear last in the returned result. *)
