(* Ocsigen
 * http://www.ocsigen.org
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

val close_service_session :
  ?state_name:string -> 
  ?scope:Eliom_common.user_scope ->
  secure: bool option ->
  ?sp:Eliom_common.server_params -> unit -> unit
val find_or_create_service_cookie :
  ?set_session_group:string ->
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure: bool option ->
  ?sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
val find_service_cookie_only :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure: bool option ->
  ?sp:Eliom_common.server_params ->
  unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
