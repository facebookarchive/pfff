(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessiongroups.ml
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

val make_full_named_group_name_ : 
  cookie_scope:Eliom_common.cookie_scope ->
  Eliom_common.sitedata ->
  string -> [< Eliom_common.scope ] Eliom_common.sessgrp

val make_full_group_name : 
  cookie_scope:Eliom_common.cookie_scope ->
  Ocsigen_extensions.request_info -> string -> 
  int32 -> int64 * int64 ->
  string option -> [< Eliom_common.scope ] Eliom_common.sessgrp

val make_persistent_full_group_name :
  cookie_scope:Eliom_common.cookie_scope ->
  string -> string option ->
  Eliom_common.perssessgrp option

val getsessgrp : 
  [< Eliom_common.scope ] Eliom_common.sessgrp -> 
  string * Eliom_common.cookie_scope *
    (string, Ocsigen_lib.ip_address) Ocsigen_lib.leftright

val getperssessgrp : Eliom_common.perssessgrp ->
  (string * Eliom_common.cookie_scope * 
     (string, Ocsigen_lib.ip_address) Ocsigen_lib.leftright)

module type MEMTAB =
  sig
    type group_of_group_data

    val add : ?set_max: int -> 
      Eliom_common.sitedata ->
      string -> [< Eliom_common.cookie_scope ] Eliom_common.sessgrp -> 
      string Ocsigen_cache.Dlist.node
    val remove : 'a Ocsigen_cache.Dlist.node -> unit
    val remove_group : [< Eliom_common.cookie_scope ] Eliom_common.sessgrp ->
      unit

    (** Groups of browser sessions belongs to a group of groups.
        As these groups are not associated to a cookie,
        we put this information here. *)
    val find_node_in_group_of_groups : 
      [< `Session ] Eliom_common.sessgrp -> 
      group_of_group_data option

    val move :
      ?set_max:int ->
      Eliom_common.sitedata ->
      string Ocsigen_cache.Dlist.node ->
      [< Eliom_common.cookie_scope ] Eliom_common.sessgrp ->
      string Ocsigen_cache.Dlist.node

    val up : string Ocsigen_cache.Dlist.node -> unit
    val nb_of_groups : unit -> int
    val group_size : [< Eliom_common.cookie_scope ] Eliom_common.sessgrp -> int
    val set_max : 'a Ocsigen_cache.Dlist.node -> int -> unit
  end

module Serv : MEMTAB with type group_of_group_data =
  (Eliom_common.tables ref *
     [ `Session ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node)

module Data : MEMTAB with type group_of_group_data =
  [ `Session ] Eliom_common.sessgrp Ocsigen_cache.Dlist.node


module Pers :
  sig
    val find : Eliom_common.perssessgrp option -> string list Lwt.t
    val add : ?set_max: int option ->
      int option -> string -> Eliom_common.perssessgrp option -> string list Lwt.t
    val remove : Eliom_common.sitedata -> 
      string -> Eliom_common.perssessgrp option -> unit Lwt.t
    val remove_group :
      cookie_scope:[ `Session | `Client_process of Eliom_common.perssessgrp option ] ->
      Eliom_common.sitedata ->
      Eliom_common.perssessgrp option -> unit Lwt.t
    val move :
      Eliom_common.sitedata ->
      ?set_max: int option ->
      int option ->
      string ->
      Eliom_common.perssessgrp option ->
      Eliom_common.perssessgrp option ->
      string list Lwt.t
    val up : string -> Eliom_common.perssessgrp option -> unit Lwt.t
    val nb_of_groups : unit -> int Lwt.t

    val close_persistent_session2 :
      cookie_scope:[ `Session | `Client_process ] ->
      Eliom_common.sitedata ->
      Eliom_common.perssessgrp option -> string -> unit Lwt.t
  end

