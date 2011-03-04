(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessadmin.ml
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

val close_all_service_sessions :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  Eliom_common.sitedata -> unit Lwt.t
val close_all_data_sessions :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  Eliom_common.sitedata -> unit Lwt.t
val close_all_persistent_sessions :
  ?state_name:string -> 
  ?cookie_scope:Eliom_common.cookie_scope ->
  Eliom_common.sitedata -> unit Lwt.t
val update_serv_exp :
  Eliom_common.fullsessionname ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_data_exp :
  Eliom_common.fullsessionname ->
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
val update_pers_exp : 
  Eliom_common.fullsessionname -> 
  Eliom_common.sitedata -> float option -> float option -> unit Lwt.t
