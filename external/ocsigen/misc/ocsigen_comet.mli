(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
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

(** Ocsigen_comet server extension : provides low-level server to client communication
    scheme. *)

module Channels :
(** A module with all the base primitive needed for server push. *)
sig

  exception Too_many_virtual_channels
  (** An exception that may be raised when trying to create a new channel while
      the channel count exceed [max_virtual_channels]. Note that by default
      [max_virtual_channels] is set to [None] so that the exception is never
      raised. *)

  exception Non_unique_channel_name
  (** An exception raised when creating a channel with a name already associated
      to another channel. It is strictly forbidden to name several channels with
      the same string. *)

  type t
  (** The abstract type of server-to-client communication channels. *)

  type chan_id = string
  (** The type of channel identifier. Channels are uniquely identified by there
      chan_id value. *)

  val create : ?name:string -> unit -> t
  (** [create ()] returns a channel with a freshly baked identifier while
      [create ~name ()] returns a channel with the identifier [name] after
      checking for uniqueness. If [name] is the identifier of an existing
      channel, the exception [Non_unique_channel_name] is raised. *)

  val write : t -> (string * Ocsigen_stream.outcome Lwt.u option) -> unit
  (** [write c (s, u)] sends the string [s] on the channel [c]. The argument [u]
      allow one to observe the result of the operation. If [u] is [None], there
      is no way to tell if the sending worked as expected. However if [u] is
      [Some u'] then [u'] will be woken up with the outcome (either [`Falure] or
      [`Success]) of the stream writing process. *)

  val listeners : t -> int
  (** [listeners c] returns the number of clients currently registered on [c]
      A client is "currently registered" on a channel if an actual
      connection is open for the server to push a message onto. Note that this
      information is server-based only, and that because it is so, some clients
      may still be registered as active while they have in fact closed the
      connection. In such a case, the outcome mechanism in [write] will report
      the failure. *)

  val get_id : t -> chan_id
  (** [get_id c] returns the unique identifier associated to [c]. The client can
      register to [c] using the returned identifier. *)

end

module Security :
(** This module is to be used carefully, it provides functions to interrupt and
    restart Comet related connections. It is however useful to prevent Comet
    based DOS attacks. These functions can also be called from the Ocsigen
    command pipe. *)
sig

  val set_timeout : ?reset:bool -> float -> unit
  (** [set_timeout ?reset f] sets the timeout value for future Comet connections
      to [f]. If [reset] is [true] then current connections are closed and the
      new timeout value will apply to the reopened connections. Default value
      for [reset] is false. *)

  val deactivate : unit -> unit
  (** [deactivate ()] ceases all Comet related activity. Each opened connection
      is closed. Further attempts to connect to the server with a Comet specific
      content type will result in a HTTP status code 503 (Unavailable).
      If called when Comet is not activated it does nothing (not even logging
      the deactivation attempt. *)

  val activate : unit -> unit
  (** [activate ()] starts serving Comet requests. It is the client's own
      responsibility to reopen a connection. If Comet was already activated it
      keeps going and nothing happens. *)

  val activated : unit -> bool
  (** [activated ()] reflects the activation state of the Comet
      module. If [false] it indicates that Comet connections are answered with a
      HTTP status code 503. If [true] it indicates that Comet connections are
      handled in a standard fashion by the server. *)

end

(** Usage:

  On the server side :
    1) create needed channels
    2) transmit their identifiers to clients
    3) write when appropriate (using the outcome mechanism if necessary

  On the client :
    1) make a XmlHttpRequest (XHR) with a list of channel identifiers.
    2) wait for the reply
    3) GOTO 1

  Encoding for client-to-server requests:
    * The content type header should be set to [application/x-ocsigen-comet]
      (without quotes)
    * A POST parameter is required. Its name should be [registration] and its
      content should be a list of channel identifiers separated by [\n]
      (newline) characters.
    * Name and content of the said POST parameter should be encoded according to
      the [escape] JavaScript primitive


  Encoding for server-to-client answer:
    * The server answer is either empty (when no channel was written upon before
      timeout) or a list of pairs of channel identifiers and message content.
      The pairs are separated by [:] (colon) while the list elements are
      separated by [\n] (newline) characters.
    * In the list, channels that no longer exists on the server side are marked
      as pairs of channel identifier and the special string [ENDED_CHANNEL].
      When receiving such a message, the client should lose hope of ever
      connecting to that particular channel ever again.

  *)
(** Conf-file options:

    One can use the configuration file to tweak Ocsigen_comet settings. The supported
    options are:

    * max_virtual_channels:
      * default: [None]
      * syntax: "" is for [None], "i" is for [Some (int_of_string i)]
      * [max_virtual_channels] is an upper limit to the number of active
        channels. It does not limit the number of connections but the number of
        values of type [Ocsigen_comet.Channels.t] that can be used simultaneously. If
        one calls [Ocsigen_comet.Channels.create] while the number of channels is
        already maxed out, the exception
        [Ocsigen_comet.Channels.Too_many_virtual_channels] is raised.

  *)
(** Commands:

    Comet provides commands (to be piped into Ocsigen's command pipe). The
    complete list of commands is described here. Don't forget to use the Comet
    prefix: each command is to be prefixed by "comet:" (without quotes).

    * deactivate:
      * deactivate is a command that stops all Comet activity. It is equivalent
        to a call to [Ocsigen_comet.Security.deactivate].

    * activate:
      * activate is the dual command to deactivate. It resumes Comet activity
        (or do nothing is Comet is already activated) with exactly the same
        effect as a call to [Ocsigen_comet.Security.activate] would have.

    * set_timeout:
      * parameter: f (float)
      * optional parameter: s ("KILL")
      * set_timeout allows one to dynamically change the value of Comet
        connections timeout to [f]. Previously activated connections are closed
        if the second optional parameter is used. If not, connections are
        carried out with their old timeout unchanged.

  *)


(** Note to Eliom users:
    Although it is possible to use Ocsigen_comet as an extension to the Ocsigen Server,
    it is recommended to use the higher level Eliom modules, namely Eliom_comet
    (for server side) and Eliom_client_comet (for client side). The former
    provides typed channels (with automatic marshaling) and channel wrapping,
    the later automates decoding and demarshaling and manages channel
    registration and deregistration.

    The low level Ocisgen server extension can however be used with classic
    Javascript clients (whereas the high level Eliom module requires Ocaml
    compatible unmarshalling which may be difficult to find in a non
    js_of_ocaml/O'browser based client). It may also be used to add your own
    high level wrapper with a custom communication protocol.
  *)
