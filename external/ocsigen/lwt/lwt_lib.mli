(* Ocsigen
 * http://www.ocsigen.org
 * lwt_lib.mli Copyright (C) 2007 Pierre Clairambault
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later version.
 * See COPYING file for details.
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

(** Cooperative unix system calls *)

(** This module transform non-cooperative functions of the standard
    library into cooperative ones by launching them into system
    threads.

    Indeed, lots of functions of the [Unix] modules, corresponding to
    functions of the standard C library may take times to
    complete. For example [gethostbyname] may use DNS resolution,
    users informations may be stored in a ldap database, ...

    Since these functions are implemented (in the standard C library)
    using blocking IOs, if you use them directly, you program may
    hang. *)

val getaddrinfo : string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list Lwt.t
  (** Cooperative getaddrinfo with cache (using Lwt_preemptive.detach) *)

val gethostbyname : string -> Unix.host_entry Lwt.t
  (** Cooperative gethostbyname with cache (using Lwt_preemptive.detach) *)

val getnameinfo : Unix.sockaddr -> Unix.getnameinfo_option list -> Unix.name_info Lwt.t
  (** Cooperative getnameinfo with cache (using Lwt_preemptive.detach) *)
