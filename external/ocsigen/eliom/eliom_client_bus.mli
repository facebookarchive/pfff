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

type 'a t

val unwrap :
    (  ('a Eliom_common_comet.chan_id)
     * (unit,
        'a list,
        [< Eliom_services.service_kind ],
        [< `WithSuffix | `WithoutSuffix ],
        'b,
        'c,
        [< Eliom_services.registrable ],
        'd) Eliom_services.service
    ) Eliom_client_types.data_key
  -> 'a t

val stream : 'a t -> 'a Lwt_stream.t
(** [stream b] returns the stream of datas sent to bus [b]. Notice you
    sould not use that function multiple times on the same bus in the
    same client process, it will return the same stream. If you want
    to receive mutiple times the same datas, you sould copy the stream
    with [Lwt_stream.clone] *)

val write : 'a t -> 'a -> unit Lwt.t
(** [write b v] send [v] to the bus [b]. Every participant of the bus
    will receive [v], including the sender. *)

val close : 'a t -> unit
(** after [close b], [stream b] stops receiving new messages from the
    bus, but it is still possible to write to the bus. It is also
    possible to close the bus by canceling a thread reading on the
    stream. *)
