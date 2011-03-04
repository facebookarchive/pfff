(* Ocsigen
 * ocsigen_stream.ml Copyright (C) 2005 Vincent Balat
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

exception Interrupted of exn
exception Cancelled
exception Already_read
exception Finalized

(** Streams are a means to read data block by block *)

type 'a stream

(** A stream may be composed by several substreams.
   Thus a stream is either something that contains the current buffer and
   a function to retrieve the following data,
   or a finished stream with possibly another stream following.
 *)
type 'a step = private
  | Finished of 'a stream option
  | Cont of 'a * 'a stream

type 'a t

type outcome = [`Success | `Failure]

(** creates a new stream *)
val make : ?finalize:(outcome -> unit Lwt.t) -> (unit -> 'a step Lwt.t) -> 'a t

(** call this function if you decide to start reading a stream.
    @raise Already_read if the stream has already been read. *)
val get : 'a t -> 'a stream

(** get the next step of a stream.
    Fails with [Interrupted e] if reading the thread failed with exception [e],
    and with [Cancelled] if the thread has been cancelled. *)
val next : 'a stream -> 'a step Lwt.t


(** creates an empty step. The parameter is the following substream, if any. *)
val empty : (unit -> 'a step Lwt.t) option -> 'a step Lwt.t

(** creates a non empty step. *)
val cont : 'a -> (unit -> 'a step Lwt.t) -> 'a step Lwt.t


(** Add a finalizer function *)
val add_finalizer : 'a t -> (outcome -> unit Lwt.t) -> unit

(** Finalize the stream *)
val finalize : 'a t -> outcome -> unit Lwt.t

(** Cancel the stream, i.e. read the stream until the end, without decoding.
    Further tries to read on the stream will fail with exception
    {!Ocsigen_stream.Cancelled}
 *)
val cancel : 'a t -> unit Lwt.t

(** Consume without cancelling.
    Read the stream until the end, without decoding. *)
val consume : 'a t -> unit Lwt.t


exception Stream_too_small
  (** possibly with the size of the stream *)

exception Stream_error of string
exception String_too_large

(** Creates a string from a stream. The first argument is the upper limit of the
    string length *)
val string_of_stream : int -> string stream -> string Lwt.t

(** Read more data in the buffer *)
val enlarge_stream : string step -> string step Lwt.t

(** [stream_want s len] Returns a stream with at least len
    bytes in the buffer if possible *)
val stream_want : string step -> int -> string step Lwt.t

(** Returns the value of the current buffer *)
val current_buffer : string step -> string

(** Skips data. Raises [Stream_too_small (Some size)] 
    if the stream is too small, where [size] is the size of the stream. *)
val skip : string step -> int64 -> string step Lwt.t

(** Cut the stream at the position given by a string delimiter *)
val substream : string -> string step -> string step Lwt.t



(*VVV à revoir : *)

(** returns a stream reading from a file.
   Do not forget to finalize the stream to close the file.
 *)
val of_file : string -> string t

(** returns a stream containing a string. *)
val of_string : string -> string t
