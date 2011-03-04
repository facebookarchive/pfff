(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_persess.ml
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

val perstables : string list ref
val persistent_cookies_table :
  (Eliom_common.fullsessionname * float option * Eliom_common.timeout *
   Eliom_common.perssessgrp option)
  Ocsipersist.table Lazy.t
val number_of_persistent_tables : unit -> int
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
val close_persistent_session2 :
  cookie_scope:Eliom_common.cookie_scope ->
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option -> string -> unit Lwt.t
val close_persistent_session :
  ?state_name:string -> 
  ?scope:Eliom_common.user_scope ->
  secure:bool option ->
  ?sp:Eliom_common.server_params -> unit -> unit Lwt.t
val find_or_create_persistent_cookie :
  ?set_session_group:string ->
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure:bool option ->
  ?sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_persistent_cookie_only :
  ?state_name:string ->
  ?cookie_scope:Eliom_common.cookie_scope ->
  secure:bool option ->
  ?sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
