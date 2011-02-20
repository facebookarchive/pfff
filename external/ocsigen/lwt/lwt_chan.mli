(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_chan
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

(** Module [Lwt_chan]: cooperative, [Pervasives]-like, I/O functions *)

(** Note: the {!Lwt_io} module deprecates this module. *)

(** {2 Cooperative input channels} *)
type in_channel = Lwt_io.input_channel

val in_channel_of_descr : Lwt_unix.file_descr -> in_channel

val make_in_channel : ?close:(unit -> unit Lwt.t) -> (string -> int -> int -> int Lwt.t) -> in_channel
(** [make_in_channel read] creates an input channel from the [read]
    function. [read s ofs len] should (cooperatively) read [len] bytes from
    the source, and put them in [s], at offset [ofs], and return the number
    of bytes effectively read. If provided, [close] will be called by
    [close_in]. By default, [close_in] does not do anything. *)

val input_line : in_channel -> string Lwt.t
val input_value : in_channel -> 'a Lwt.t
val input : in_channel -> string -> int -> int -> int Lwt.t
val really_input : in_channel -> string -> int -> int -> unit Lwt.t
val input_char : in_channel -> char Lwt.t
val input_binary_int : in_channel -> int Lwt.t

val open_in_gen : Unix.open_flag list -> int -> string -> in_channel Lwt.t
val open_in : string -> in_channel Lwt.t
val close_in : in_channel -> unit Lwt.t

(** {2 Cooperative output channels} *)

type out_channel = Lwt_io.output_channel

val out_channel_of_descr : Lwt_unix.file_descr -> out_channel

val make_out_channel : ?close:(unit -> unit Lwt.t) -> (string -> int -> int -> int Lwt.t) -> out_channel
(** [make_out_channel write] creates an output channel from the [write]
    function. [write s ofs len] should (cooperatively) write [len] bytes from
    [s], starting at offset [ofs], to the backend, and return the number of
    bytes effectively written. If provided, [close] will be called by
    [close_out]. By default, [close_out] does not do anything. *)

val output : out_channel -> string -> int -> int -> unit Lwt.t
val flush : out_channel -> unit Lwt.t
val output_string : out_channel -> string -> unit Lwt.t
val output_value : out_channel -> 'a -> unit Lwt.t
val output_char : out_channel -> char -> unit Lwt.t
val output_binary_int : out_channel -> int -> unit Lwt.t

val open_out_gen : Unix.open_flag list -> int -> string -> out_channel Lwt.t
val open_out : string -> out_channel Lwt.t
val close_out : out_channel -> unit Lwt.t

val open_connection : Unix.sockaddr -> (in_channel * out_channel) Lwt.t
