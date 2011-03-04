(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_pagegen.ml
 * Copyright (C) 2007 Vincent Balat
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
val def_handler : exn -> 'b Lwt.t
val handle_site_exn :
  exn ->
  Eliom_common.info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
val execute :
  float ->
  (float ->
  Eliom_common.info ->
   Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t) ->
  Eliom_common.info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
val gen :
  Eliom_extensions.eliom_extension_sig option ->
  Eliom_common.sitedata ->
  Ocsigen_extensions.request_state -> Ocsigen_extensions.answer Lwt.t
val update_cookie_table :
  ?now:float ->
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  unit Lwt.t
