(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessexpl.ml
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

val iter_service_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key *
   Eliom_common.tables Eliom_common.servicecookiestablecontent *
   Eliom_common.sitedata -> unit Lwt.t) ->
  unit Lwt.t
val iter_data_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key * Eliom_common.datacookiestablecontent *
   Eliom_common.sitedata -> unit Lwt.t) ->
  unit Lwt.t
val iter_persistent_sessions :
  (string *
   (Eliom_common.fullsessionname * float option * Eliom_common.timeout *
    Eliom_common.perssessgrp option) ->
   unit Lwt.t) ->
  unit Lwt.t
val fold_service_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key *
   Eliom_common.tables Eliom_common.servicecookiestablecontent *
   Eliom_common.sitedata -> 'a -> 'a Lwt.t) ->
  'a -> 'a Lwt.t
val fold_data_sessions :
  Eliom_common.sitedata ->
  (Eliom_common.SessionCookies.key * Eliom_common.datacookiestablecontent *
   Eliom_common.sitedata -> 'a -> 'a Lwt.t) ->
  'a -> 'a Lwt.t
val fold_persistent_sessions :
  (string *
   (Eliom_common.fullsessionname * float option * Eliom_common.timeout *
    Eliom_common.perssessgrp option) ->
   'a -> 'a Lwt.t) ->
  'a -> 'a Lwt.t
val number_of_service_sessions : unit -> int
val number_of_data_sessions : unit -> int
val number_of_tables : unit -> int
val number_of_table_elements : unit -> int list
val number_of_persistent_sessions : unit -> int Lwt.t
