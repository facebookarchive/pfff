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


val make_a_with_onclick :
  (?a:'a -> ?onclick:XML.event -> 'c -> 'd) ->
  ('d -> string -> (unit -> unit Lwt.t) -> unit -> 'f) ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:'a ->
  service:('get, unit, [< Eliom_services.get_service_kind ],
           [< Eliom_services.suff ], 'gn, 'pn,
           [< Eliom_services.registrable ], 'return)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  'c -> 'get -> 'd



val make_get_form_with_onsubmit :
  (?a:'a -> action:'b -> ?onsubmit:XML.event -> 'c -> 'd -> 'form) ->
  ('form -> string -> (unit -> unit Lwt.t) -> 'g) ->
  ('form -> unit -> unit Lwt.t) ->
  string ->
  ?a:'a -> action:'b -> 'c -> 'd -> 'form

val make_post_form_with_onsubmit :
  (?a:'a -> action:'b -> ?onsubmit:XML.event ->
   ?id:string ->
   ?inline:bool ->
   'c -> 'd -> 'form) ->
  ('form -> string -> (unit -> unit Lwt.t) -> 'g) ->
  ('form -> unit -> unit Lwt.t) ->
  string ->
  ?a:'a -> action:'b -> 'c -> 'd -> 'form

val add_tab_cookies_to_get_form : 'form XHTML5.M.elt -> unit -> unit Lwt.t
val add_tab_cookies_to_post_form : 'form XHTML5.M.elt -> unit -> unit Lwt.t
