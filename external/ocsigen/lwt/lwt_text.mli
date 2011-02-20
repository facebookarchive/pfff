(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_text
 * Copyright (C) 2009 Jérémie Dimino
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

(** Text channels *)

(** This modules implements {b text channel}s. A {b text channel} is
    basically a {b byte channel} (as in {!Lwt_io}) plus a {b character
    encoding}.

    It has almost the same interface as {!Lwt_io} except that it
    uses [Text.t] in place of [string] and [char]
*)

open Lwt_io

(** {6 Types} *)

type 'mode channel
  (** Type of a text channel *)

type input_channel = input channel
    (** Type of a text input channel *)

type output_channel = output channel
    (** Type of a text output channel *)

(** {6 Creation/manipulation} *)

val make : ?strict : bool -> ?encoding : Encoding.t -> 'a Lwt_io.channel -> 'a channel
  (** [make ?strict ?encoding ch] creates a text channel from a byte
      channel.

      @param strict tell whether encoding/decoding must be ``strict'',
      which whether the encoder/decoder should fail on invalid
      sequence. In non-strict mode, it transparently fallback to
      ISO-8859-15. By the way it is ensured that [read*] functions
      always returns valid UTF-8 encoded text. [strict] defaults to
      [false].

      @param encoding is the character encoding used for the
      channel. It defaults to [Encoding.system]. *)

val byte_channel : 'a channel -> 'a Lwt_io.channel
  (** [byte_channel ch] returns the underlying byte channel of a text
      channel *)

val encoding : 'a channel -> Encoding.t
  (** [encoding ch] returns the character encoding of a channel. *)

val flush : output_channel -> unit Lwt.t
  (** Flush the underlying byte channel *)

val close : 'a channel -> unit Lwt.t
  (** Close the underlying byte channel *)

(** {6 Lwt_io like values} *)

val atomic : ('a channel -> 'b Lwt.t) -> ('a channel -> 'b Lwt.t)
val stdin : input_channel
val stdout : output_channel
val stderr : output_channel
val zero : input_channel
val null : output_channel
val read_char : input_channel -> Text.t Lwt.t
val read_char_opt : input_channel -> Text.t option Lwt.t
val read_chars : input_channel -> Text.t Lwt_stream.t
val read_line : input_channel -> Text.t Lwt.t
val read_line_opt : input_channel -> Text.t option Lwt.t
val read_lines : input_channel -> Text.t Lwt_stream.t
val read : ?count : int -> input_channel -> Text.t Lwt.t
val write_char : output_channel -> Text.t -> unit Lwt.t
val write_chars : output_channel -> Text.t Lwt_stream.t -> unit Lwt.t
val write : output_channel -> Text.t -> unit Lwt.t
val write_line : output_channel -> Text.t -> unit Lwt.t
val write_lines : output_channel -> Text.t Lwt_stream.t -> unit Lwt.t
val open_file :
  ?buffer_size : int ->
  ?strict : bool ->
  ?encoding : Encoding.t ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a mode ->
  file_name -> 'a channel Lwt.t
val with_file :
  ?buffer_size : int ->
  ?strict : bool ->
  ?encoding : Encoding.t ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a mode ->
  file_name -> ('a channel -> 'b Lwt.t) -> 'b Lwt.t
val lines_of_file : file_name -> Text.t Lwt_stream.t
val lines_to_file : file_name -> Text.t Lwt_stream.t -> unit Lwt.t
val chars_of_file : file_name -> Text.t Lwt_stream.t
val chars_to_file : file_name -> Text.t Lwt_stream.t -> unit Lwt.t
val fprint : output_channel -> Text.t -> unit Lwt.t
val fprintl : output_channel -> Text.t -> unit Lwt.t
val fprintf : output_channel -> ('a, unit, Text.t, unit Lwt.t) format4 -> 'a
val fprintlf : output_channel -> ('a, unit, Text.t, unit Lwt.t) format4 -> 'a
val print : Text.t -> unit Lwt.t
val printl : Text.t -> unit Lwt.t
val printf : ('a, unit, Text.t, unit Lwt.t) format4 -> 'a
val printlf : ('a, unit, Text.t, unit Lwt.t) format4 -> 'a
val eprint : Text.t -> unit Lwt.t
val eprintl : Text.t -> unit Lwt.t
val eprintf : ('a, unit, Text.t, unit Lwt.t) format4 -> 'a
val eprintlf : ('a, unit, Text.t, unit Lwt.t) format4 -> 'a
val hexdump_stream : output_channel -> char Lwt_stream.t -> unit Lwt.t
val hexdump : output_channel -> string -> unit Lwt.t
