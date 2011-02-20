(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_ssl
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** OCaml-SSL integration *)

type socket

val ssl_accept : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val ssl_connect : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val plain : Lwt_unix.file_descr -> socket
val embed_socket : Lwt_unix.file_descr -> Ssl.context -> socket

val read : socket -> string -> int -> int -> int Lwt.t
val write : socket -> string -> int -> int -> int Lwt.t

val read_bytes : socket -> Lwt_bytes.t -> int -> int -> int Lwt.t
val write_bytes : socket -> Lwt_bytes.t -> int -> int -> int Lwt.t

(* Really wait on a plain socket, just yield over SSL *)
val wait_read : socket -> unit Lwt.t
val wait_write : socket -> unit Lwt.t

val shutdown : socket -> Unix.shutdown_command -> unit
val close : socket -> unit Lwt.t

val out_channel_of_descr : socket -> Lwt_chan.out_channel
val in_channel_of_descr : socket -> Lwt_chan.in_channel

val ssl_shutdown : socket -> unit Lwt.t

val abort : socket -> exn -> unit

(** Are we using an SSL socket? *)
val is_ssl : socket -> bool

val getsockname : socket -> Unix.sockaddr

val getpeername : socket -> Unix.sockaddr
