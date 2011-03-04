(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
 * Pierre Chambart
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

(** This module contains functions to handle unsolicited server to
    client communication on the client side *)

(** when the page is not active the client stops making comet requests
    to the server, implying that the client can't be notified by the
    server anymore. The activity status is changed when the page is
    focused or unfocused.

    To stop receiving inputs from a channel, use Lwt.cancel on a
    thread waiting for datas. For instance, if you iterate with
    [ let t = Lwt_stream.iter f %channel ] calling [Lwt.cancel t]
    will close the channel. *)

val is_active : unit -> bool
(** [is_active ()] returns the current activity state *)

val activate : unit -> unit
(** if the client is inactive [activate ()] launch a new xhr
    connection to start receiving server messages *)

module Configuration :
sig
  (** This modules is used to change the reactivity of channels.
      Multiples configurations ( of type [t] ) can be created. The
      resulting behaviour is the minimal ( in the meaning of maximal
      reactivity ) between all configurations *)
  
  type t

  val new_configuration : unit -> t
  (** Creates a new configuration with default value. It modifies the
      current behaviour immediately *)

  val drop_configuration : t -> unit
  (** [drop_configuration t] restores the behaviour to the minimum of
      configuration without [t]. If there is no other configuration
      than [t], it is restored to the defaults. *)

  val set_always_active : t -> bool -> unit
  (** [set_always_active c true] tells the client to always stay active.
      Default value is false. *)

  val set_active_until_timeout : t -> bool -> unit
  (** [set_active_until_timeout c v] sets the activity changing
      behaviour. if [v] is [true] the page is kept active even if not
      focused until the client receive a timeout message from the
      server. It implies that if the server keeps sending datas to the
      client, the comet connection will never be closed.
      Default value is false. *)

  val set_time_between_request : t -> float -> unit
  (** after [set_time_between_request t v], the main loop will wait for
      [v] seconds between two requests. It is taken into account
      immediately.
      Default value is 0.*)

end

(**/**)

val unwrap : ?wake:bool -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key -> 'a Lwt_stream.t
val register : ?wake:bool -> 'a Eliom_common_comet.chan_id -> 'a Lwt_stream.t
(** if wake is false, the registration of the channel won't
    activate the handling loop ( no request will be sent ). Default is true *)

val restart : unit -> unit
(** [restart ()] Restarts the loop waiting for server messages. It is
    only usefull after that a formulary is sent. Indeed browsers stops
    all xhr requests in that case. It is normaly not needed, but some
    brosers (based on webkit) also destroy the xhr object in that
    case, preventing client code from receiving the failure
    notification. This shouldn't be used by average user. *)

val close : 'a Eliom_common_comet.chan_id -> unit
(** [close c] closes the channel c. This function should be only use
    internaly. The normal way to close a channel is to cancel a thread
    waiting on inputs. *)
