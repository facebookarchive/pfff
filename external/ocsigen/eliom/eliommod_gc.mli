(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_gc.ml
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

val servicesessiongcfrequency : float option ref
val datasessiongcfrequency : float option ref
val persistentsessiongcfrequency : float option ref
val set_servicesessiongcfrequency : float option -> unit
val set_datasessiongcfrequency : float option -> unit
val get_servicesessiongcfrequency : unit -> float option
val get_datasessiongcfrequency : unit -> float option
val set_persistentsessiongcfrequency : float option -> unit
val get_persistentsessiongcfrequency : unit -> float option
val service_session_gc : Eliom_common.sitedata -> unit
val data_session_gc : Eliom_common.sitedata -> unit
val persistent_session_gc : Eliom_common.sitedata -> unit
