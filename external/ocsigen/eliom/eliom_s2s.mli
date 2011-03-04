(* Ocsigen
 * Copyright (C) 2010 Simon Castellan
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

(** Server-to-Server communication module.
    Provides general function to communicate to other servers using
    post or get request. *)

(** Format a url with a base and GET parameters *)
val format_url : string -> (string * string) list -> string
(** Do a get request towards a given url *)
val do_get_request : ?params : (string * string) list -> string -> Ocsigen_http_frame.t Lwt.t

(** Given a list of [(param, value)] to be sent to the remote server,
    push a namespace at the beginning of the parameters *)
val push_ns :
  ?sep:string ->
  ?namespace_param:string * 'a ->
  string -> (string * 'a) list -> (string * 'a) list

(** Finds some parameters in a namespace, identified by its url.
    You can specify a default namespace, so [find_in_ns] do not fail
    when the url is not found. *)
val find_in_ns :
  ?namespace_param:string ->
  ?default_namespace:string -> 'a -> (string * 'a) list -> (string * 'a) list

(** Retrieves parameters beginning with the specified namespace and
    strips it. *)
val strip_ns : string -> (string * string) list -> (string * string) list
(** Parse an answer in the Key-Value form :
{v foo:bar
foobar:value v} *)
val parse_key_pairs : string -> (string * string) list

(** Perform a direct (POST) request towards a server,
    and parse the result as key-value data *)
val direct_request : (string * string) list -> string -> (string * string) list Lwt.t

