(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

(*****************************************************************************)
(** {2 Server side state data: Eliom references} *)

(** {e Warning: Eliom references of scope [`Global] or [`Request] may be created
    and accessed at any time.
    For other scopes, they must be created or accessed when the site
    information is available to Eliom, that is, either during the initialization
    phase of the server (while reading the configuration file) or during
    a request. Otherwise, it will raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.
    If you are using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_services.register_eliom_module}. Otherwise you will also get 
    this exception.}
*)

(** The type of Eliom references. *)
type 'a eref

(** Create an Eliom reference for the given scope (default: [`Global]).

    Use the optional parameter [?persistent] if you want the data to survive
    after relaunching the server. You must give an unique name to the
    table in which it will be stored on the hard disk (using Ocsipersist).
    Be very careful to use unique names, and to change the name if
    you change the type of the data, otherwise the server may crash
    (unsafe unmarshaling).
    This parameter has no effect for scope [`Request].

    Use the optional parameter [?secure] if you want the data to be available
    only using HTTPS (default: false). It has no effect for scopes [`Global]
    and [`Request].

    Use the optional parameter [?state_name] if you want to distinguish
    between several server side states for the same scope.
    It has no effect for scopes [`Global] and [`Request].
*)
val eref :
  ?state_name:string ->
  ?scope:[ `Request | Eliom_common.scope ] ->
  ?secure:bool ->
  ?persistent:string ->
  'a -> 'a eref

(** Get the value of an Eliom reference. *)
val get : 'a eref -> 'a Lwt.t
(* That function introduces a Lwt cooperation point only for persistent
   references. *)

(** Change the value of an Eliom reference. *)
val set : 'a eref -> 'a -> unit Lwt.t
(* That function introduces a Lwt cooperation point on for persistent
   references. *)


(** Turn back to the default value 
    (by removing the entry in the server side table in the case where
    they are stored in a table).
*)
val unset : 'a eref -> unit Lwt.t
(* That function introduces a Lwt cooperation point on for persistent
   references. *)
