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


(** This module is the client part of the shared event scheme used in Eliom. It
    is to be used with [Eliom_event] as it defines an unwrapping function for
    each possible wrap in the dual module. *)


module Down :
sig

  val unwrap :
    ?wake:bool -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key
    -> 'a React.E.t

end

module Up :
sig

  val unwrap :
    (unit,
     'a,
     [< Eliom_services.service_kind ],
     [< `WithSuffix | `WithoutSuffix ],
     'b,
     'c,
     [< Eliom_services.registrable ],
     'd)
        Eliom_services.service Eliom_client_types.data_key
  -> ('a -> unit Lwt.t)
  (** [unwrap e] returns a function that, when called, triggers the transmitted
      event on the server. The thread returns when the write is done. *)

end
