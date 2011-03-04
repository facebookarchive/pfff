(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

exception Looping_redirection
exception Failed_request of int
exception Program_terminated
exception External_service

val redirect_get : string -> unit
val redirect_post : string -> (string * string) list -> unit

val send :
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * string) list -> string -> string Lwt.t

val http_get :
  ?cookies_info:bool option *
  ('a, 'b,
   [< `Attached of ([> `External ], 'c) Eliom_services.a_s
   | `Nonattached of 'd ],
   [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
                Eliom_services.service * 'a ->
  string ->
  (string * string) list -> string Lwt.t

val http_post :
  ?cookies_info:bool option *
                ('a, 'b,
                 [< `Attached of ([> `External ], 'c) Eliom_services.a_s
                  | `Nonattached of 'd ],
                 [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
                Eliom_services.service * 'a ->
  string ->
  (string * string) list -> string Lwt.t

val get_eliom_appl_result : string -> Eliom_services.eliom_appl_answer

val get_cookie_info_for_uri_js : Js.js_string Js.t -> bool * string list

val max_redirection_level : int
