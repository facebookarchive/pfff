(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_unix
 * Copyright (C) 2010 Jérémie Dimino
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

(** Byte arrays *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** Type of array of bytes. *)

val create : int -> t
  (** Creates a new byte array of the given size. *)

val length : t -> int
  (** Returns the length of the given byte array. *)

(** {6 Access} *)

external get : t -> int -> char = "%caml_ba_ref_1"
  (** [get buffer offset] returns the byte at offset [offset] in
      [buffer]. *)

external set : t -> int -> char -> unit = "%caml_ba_set_1"
  (** [get buffer offset value] changes the value of the byte at
      offset [offset] in [buffer] to [value]. *)

external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
  (** Same as {!get} but without bound checking. *)

external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
  (** Same as {!set} but without bound checking. *)

(** {6 Conversions} *)

val of_string : string -> t
  (** [of_string str] returns a newly allocated byte array with the
      same contents as [str]. *)

val to_string : t -> string
  (** [to_string buf] returns a newly allocated string with the same
      contents as [buf]. *)

(** {6 Copying} *)

val blit : t -> int -> t -> int -> int -> unit
  (** [blit buf1 ofs1 buf2 ofs2 len] copy [len] bytes from [buf1]
      starting at offset [ofs1] to [buf2] starting at offset [ofs2]. *)

val blit_string_bytes : string -> int -> t -> int -> int -> unit
  (** Same as blit but the first buffer is a string instead of a byte
      array. *)

val blit_bytes_string : t -> int -> string -> int -> int -> unit
  (** Same as blit but the second buffer is a string instead of a byte
      array. *)

external unsafe_blit : t -> int -> t -> int -> int -> unit = "lwt_unix_blit_bytes_bytes" "noalloc"
  (** Same as {!blit} but without bound checking. *)

external unsafe_blit_string_bytes : string -> int -> t -> int -> int -> unit = "lwt_unix_blit_string_bytes" "noalloc"
  (** Same as {!blit_string_bytes} but without bound checking. *)

external unsafe_blit_bytes_string : t -> int -> string -> int -> int -> unit = "lwt_unix_blit_bytes_string" "noalloc"
  (** Same as {!blit_bytes_string} but without bound checking. *)

val proxy : t -> int -> int -> t
  (** [proxy buffer offset length] creates a ``proxy''. The returned
      byte array share the data of [buffer] but with different
      bounds. *)

val extract : t -> int -> int -> t
  (** [extract buffer offset length] creates a new byte array of
      length [length] and copy the [length] bytes of [buffer] at
      [offset] into it.  *)

val copy : t -> t
  (** [copy buffer] creates a copy of the given byte array. *)

(** {6 Filling} *)

val fill : t -> int -> int -> char -> unit
  (** [fill buffer offset length value] puts [value] in all [length]
      bytes of [buffer] starting at offset [offset]. *)

external unsafe_fill : t -> int -> int -> char -> unit = "lwt_unix_fill_bytes" "noalloc"
  (** Same as {!fill} but without bound checking. *)

(** {6 IOs} *)

(** The following functions does the same as the functions in
    {!Lwt_unix} except that they use byte arrays instead of
    strings. *)

val read : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t
val write : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t

val recv : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int Lwt.t
val send : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int Lwt.t

val recvfrom : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> (int * Unix.sockaddr) Lwt.t
val sendto : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int Lwt.t

type io_vector = {
  iov_buffer : t;
  iov_offset : int;
  iov_length : int;
}

val io_vector : buffer : t -> offset : int -> length : int -> io_vector

val recv_msg : socket : Lwt_unix.file_descr -> io_vectors : io_vector list -> (int * Unix.file_descr list) Lwt.t
  (** This call is not available on windows. *)

val send_msg : socket : Lwt_unix.file_descr -> io_vectors : io_vector list -> fds : Unix.file_descr list -> int Lwt.t
  (** This call is not available on windows. *)

(** {6 Memory mapped files} *)

val map_file : fd : Unix.file_descr -> ?pos : int64 -> shared : bool -> ?size : int -> unit -> t
  (** [map_file ~fd ?pos ~shared ?size ()] maps the file descriptor
      [fd] to an array of bytes. *)

external mapped : t -> bool = "lwt_unix_mapped" "noalloc"
  (** [mapped buffer] returns [true] iff [buffer] is a memory mapped
      file. *)

(** Type of advise that can be sent to the kernel by the program. See
    the manual madvise(2) for a description of each advices. *)
type advice =
  | MADV_NORMAL
  | MADV_RANDOM
  | MADV_SEQUENTIAL
  | MADV_WILLNEED
  | MADV_DONTNEED

val madvise : t -> int -> int -> advice -> unit
  (** [madvise buffer pos len advice] advise the kernel about how the
      program is going to use the part of the memory mapped file
      between [pos] and [pos + len].

      This call is not available on windows. *)

val page_size : int
  (** Size of pages. *)

val mincore : t -> int -> bool array -> unit
  (** [mincore buffer offset states] tests whether the given pages are
      in the system memory (the RAM). The [offset] argument must be a
      multiple of {!page_size}. [states] is used to store the result;
      each cases is [true] if the corresponding page in the RAM and
      [false] otherwise.

      This call is not available on windows. *)

val wait_mincore : t -> int -> unit Lwt.t
  (** [wait_mincore buffer offset] waits until the page containing the
      byte at offset [offset] in the the RAM.

      This functions is not available on windows. *)
