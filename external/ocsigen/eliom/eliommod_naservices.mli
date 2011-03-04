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

val add_naservice :
  Eliom_common.tables ->
  Eliom_common.NAserv_Table.key ->
  int ref option * (float * float ref) option *
    (Eliom_common.server_params -> Ocsigen_http_frame.result Lwt.t) ->
  unit
val remove_naservice :
  Eliom_common.tables -> Eliom_common.NAserv_Table.key -> unit
val make_naservice :
  float ->
  Eliom_common.info ->
  Eliom_common.sitedata -> Ocsigen_http_frame.result Lwt.t
