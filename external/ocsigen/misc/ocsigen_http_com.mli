(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_http_com.ml Copyright (C) 2005
 * Denis Berthod, Vincent Balat, Jérôme Vouillon
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

(** Sending and receiving HTTP frames *)

(*
Who can raise the exceptions:
R: receiver
r: receiver stream
S: sender
*)

(** The other side has cleanly closed the connection after a message *)
exception Connection_closed (* R *)

(** The connection has been unexpectedly broken *)
exception Lost_connection of exn (* RrS *)

(** No activity on the other side *)
exception Timeout (* RrS *)
exception Keepalive_timeout (* R *)

(** Connection killed *)
exception Aborted (* RrS *)

type mode = Answer | Query | Nofirstline
type connection
val create_receiver : int -> mode -> Lwt_ssl.socket -> connection
val lock_receiver : connection -> unit Lwt.t
val unlock_receiver : connection -> unit
val wakeup_next_request : connection -> unit
val block_next_request : connection -> unit Lwt.t
val get_http_frame : ?head:bool -> connection -> Ocsigen_http_frame.t Lwt.t
val connection_id : connection -> int
val connection_fd : connection -> Lwt_ssl.socket

(****)

type slot

val start_processing : connection -> (slot -> unit Lwt.t) -> unit
val wait_all_senders : connection -> unit Lwt.t

(****)

(**
This function may return any I/O error from the channel, or a
interrupted stream exception.
*)
val write_stream :
  ?chunked:bool -> Lwt_chan.out_channel -> string Ocsigen_stream.t -> unit Lwt.t

(****)

type sender_type

val create_sender :
  ?server_name:string -> ?headers:Http_headers.t ->
  ?proto:Ocsigen_http_frame.Http_header.proto -> unit -> sender_type

(** Sender with only the server name, and HTTP/1.1 *)
val default_sender : sender_type



(** send an HTTP message.
    [send] may also fail with [Interrupted_stream] exception if the input
    stream is interrupted.
 *)
val send :
    ?reopen:(unit -> unit Lwt.t) ->
    slot ->
    clientproto:Ocsigen_http_frame.Http_header.proto ->
    ?mode:Ocsigen_http_frame.Http_header.http_mode ->
    ?proto:Ocsigen_http_frame.Http_header.proto ->
    ?keep_alive:bool ->
    head:bool ->
    sender:sender_type ->
    Ocsigen_http_frame.result ->
    unit Lwt.t

val abort : connection -> unit


(** Use this function to make an action just before sending the result 
    (for example observe the headers that will be sent).
    The parameter is a function taking the set of headers twice,
    first as [Ocsigen_http_frame.Http_headers.http_header], 
    second as a [string].
*)
val set_result_observer : 
  (Ocsigen_http_frame.Http_header.http_header -> string -> unit Lwt.t) -> unit
