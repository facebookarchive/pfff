(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module Types : sig

  (** The abstract type of user ids *)
  type userid = [`User ] Opaque.int32_t

  val userid_from_sql : int32 -> userid
  val sql_from_userid : userid -> int32

  val string_from_userid : userid -> string

  type pwd =
    | Connect_forbidden
    | Ocsimore_user_plain of string
    | Ocsimore_user_crypt of string
    | External_Auth


  (** Description of the parameter of a paramaterized group *)
  type find_param = {
    (** Description of what the user is expected to type between the
        parentheses to give the name of the role, e.g. 'Name of the wiki' *)
    param_description: string;

    (** How to display the parameter when printing the description
        of the role, e.g. transforming the wiki id into the real name
        of the wiki *)
    param_display: (int32 -> string Lwt.t) option;

    (** Conversion functions to parse and unparse the content of the
        parentheses in the name of the group. Must obviously correspond
        to param_description *)
    find_param_functions:
      ((string -> int32 Lwt.t) * (int32 -> string Lwt.t)) option;
  }


  type userdata = {
    user_id: userid;
    user_login: string;
    user_pwd: pwd;
    user_fullname: string;
    user_email: string option;
    user_dyn: bool;
    user_kind: [ `BasicUser
               | `ParameterizedGroup of find_param option
               | `NonParameterizedGroup];
  }


  type 'a parameterized_group

  type user

  val apply_parameterized_group:
    'a parameterized_group ->'a Opaque.int32_t -> user
  val ($) : 'a parameterized_group ->'a Opaque.int32_t -> user
  val basic_user : userid -> user

  (** Converts an user back into an userid, if possible *)
  val is_basic_user : user -> userid option

  (** Returns [Some v] is [user] is [group $ v], or [None] otherwise *)
  val user_is_applied_parameterized_group :
    user:user -> pgroup:'a parameterized_group -> 'a Opaque.int32_t option


  type 'a admin_writer_reader = {
    grp_admin: 'a parameterized_group;
    grp_writer: 'a parameterized_group;
    grp_reader: 'a parameterized_group;
  }

end

(** Exception raised when a string cannot be translated into a real user *)
exception NotAnUser


open Types


(** Creates a user. The password passed as argument must be unencrypted.
    Returns the user id and its password after an eventual encryption. *)
val new_user:
  name:string ->
  password:pwd ->
  fullname:string ->
  email:string option ->
  dyn:bool ->
  (userid * pwd) Lwt.t


val new_parameterized_group:
  prefix:string ->
  name:string ->
  descr:string ->
  find_param:find_param ->
  'a parameterized_group Lwt.t

val new_nonparameterized_group:
  prefix:string ->
  name:string ->
  descr:string ->
  user Lwt.t



exception NotBasicUser of userdata

(* Returns the id of the user whose login is passed as argument. Raises
   [NotBasicUser] if the resulting user is not a basic user. *)
val get_basicuser_by_login: string -> userid Lwt.t

val get_basicuser_data : userid -> userdata Lwt.t
val get_parameterized_user_data: 'a parameterized_group -> userdata Lwt.t
val get_user_data : user -> userdata Lwt.t


(** Returns the groups in which a user is directly included *)
val groups_of_user : user:user -> user list Lwt.t

(** Returns the users or groups inside a group. If [generic] is false,
    inclusions coming from generic edges are not returned *)
val users_in_group : ?generic:bool -> group:user -> user list Lwt.t


val add_to_group: user:user -> group:user -> unit Lwt.t
val remove_from_group: user:user -> group:user -> unit Lwt.t

val add_generic_inclusion :
  subset:'a parameterized_group -> superset:'a parameterized_group -> unit Lwt.t

(** Same as [add_generic_inclusion]. Use the one you prefer. *)
val add_to_group_generic :
  user:'a parameterized_group -> group:'a parameterized_group -> unit Lwt.t

val delete_user: userid:userid -> unit Lwt.t



val update_data:
  userid:userid ->
  ?password:pwd ->
  ?fullname:string ->
  ?email:string ->
  ?dyn:bool ->
  unit ->
  unit Lwt.t


(** Converts an [userid] to a string, by giving the corresponding
    login field. Raises [Not_found] if the user does not exists. *)
val userid_to_string: userid -> string Lwt.t

(** Converts an user to a string. Basic users are converted as
    per [userid_to_string]. Groups are written  [#group(val)]
    where [group] is the name used at the creation
    of the group, and val is the [int32] parameter of the group.
    If [expand_param] is [true] the function tries to convert [int32] into
    a string, using the functions passed as arguments when [#group] was
    defined. *)
val user_to_string: ?expand_param:bool -> user -> string Lwt.t

(** Returns the user that corresponds to a given string
    (inverse of the function [user_to_string], or raises
    [Not_found] if the user does not exists *)
val get_user_by_name: string -> user Lwt.t


(** Returns a list of all the existing users and groups. *)
val all_groups : unit -> userdata list Lwt.t


val user_type: user -> [ `User | `Group | `Role ] Lwt.t
