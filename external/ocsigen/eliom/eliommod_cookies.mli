(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_cookies.ml
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

val make_new_session_id : unit -> string

val get_cookie_info :
  float ->
  Eliom_common.sitedata ->
  Eliom_common.SessionCookies.key Eliom_common.Fullsessionname_Table.t ->
  Eliom_common.SessionCookies.key Eliom_common.Fullsessionname_Table.t ->
  string Eliom_common.Fullsessionname_Table.t ->
  (Eliom_common.SessionCookies.key Eliom_common.Fullsessionname_Table.t *
     Eliom_common.SessionCookies.key Eliom_common.Fullsessionname_Table.t *
     string Eliom_common.Fullsessionname_Table.t) option ->
  Eliom_common.tables Eliom_common.cookie_info *
  Eliom_common.Fullsessionname_Table.key list

val new_service_cookie_table :
  unit -> Eliom_common.tables Eliom_common.servicecookiestable
val new_data_cookie_table : unit -> Eliom_common.datacookiestable
val compute_session_cookies_to_send :
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookies.cookieset -> 
  Ocsigen_cookies.cookieset Lwt.t
val compute_cookies_to_send :
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookies.cookieset -> 
  Ocsigen_cookies.cookieset Lwt.t
val compute_new_ri_cookies :
  float ->
  string list ->
  string Ocsigen_lib.String_Table.t ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookies.cookieset -> string Ocsigen_lib.String_Table.t Lwt.t
