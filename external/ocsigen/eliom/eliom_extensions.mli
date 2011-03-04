(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2008 Vincent Balat
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

(** Type of the function that must be registered to declare an eliom extension *)
type eliom_extension_sig = unit -> Ocsigen_extensions.answer Lwt.t

val register_eliom_extension : eliom_extension_sig -> unit

(**/**)

val get_eliom_extension : unit -> eliom_extension_sig

val run_eliom_extension :
  eliom_extension_sig ->
  float ->
  (Ocsigen_extensions.request * 
     Eliom_common.sess_info *
     Eliom_common.tables Eliom_common.cookie_info *
     Eliom_common.tables Eliom_common.cookie_info *
     Ocsigen_cookies.cookieset) ->
  Eliom_common.sitedata ->
  Ocsigen_extensions.answer Lwt.t
