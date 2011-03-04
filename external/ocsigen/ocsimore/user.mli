(** Module Users.

    Users, authentication, protection.

    In this model, users and groups are the same concept. A group can
    belong to another group. We only distinguish, for practical
    matters, between "login enabled" users and "group only" users: the
    former has [Some] (eventually void) password, the latter has
    [None].

*)
open User_sql.Types

exception ConnectionRefused
exception BadPassword
exception BadUser
exception UnknownUser of string
exception UseAuth of userid

(** Non authenticated users *)
val anonymous : userid
val anonymous_login: string

(** A user that belongs to all groups *)
val admin : userid
val admin_login: string

(** A user/group that does not belong to any group,  and in which nobody
    can be.  *)
val nobody : userid
val nobody_login: string

(** A group containing all authenticated users (not groups) *)
val authenticated_users : userid


(** The groups of users that can create new groups *)
val group_can_create_groups : user

(** Same thing with users *)
val group_can_create_users : user

(** The group of users that can add or remove people in the given user/group *)
val group_can_admin_group : [`User] parameterized_group




(** Information about a user. Return [nobody] if the user
    does not currently exists, and raises [User_sql.NotBasicUser]
    if the user does not correspond to a basic user. *)
val get_basicuser_by_login : string -> userid Lwt.t

(** Returns the user that corresponds to a given string
    (inverse of the function [User_sql.user_to_string],
    or nobody if the user does not exists *)
val get_user_by_name: string -> user Lwt.t

(** Convert a list of string representation of users into the
    corresponding users, according to [get_user_by_name]. Nobody
    is never returned. Fails with [UnknownUser u] if the user
    [u] is not recognized *)
val user_list_of_string : string -> user list Lwt.t


(** Creates a new user or group with given parameters,
    or returns the existing user without modification
    if [name] is already present. *)
val create_user:
  name:string ->
  pwd:pwd ->
  fullname:string ->
  ?email:string ->
  ?test:(sp:Eliom_sessions.server_params -> bool Lwt.t) ->
  unit ->
  userid Lwt.t


(** Same as above, except that the function will raise [BadUser] if
    the user already exists *)
val create_fresh_user:
  name:string ->
  pwd:pwd ->
  fullname:string ->
  ?email:string ->
  unit ->
  userid Lwt.t


val authenticate : name:string -> pwd:string -> userdata Lwt.t


(** Atomic change in one group *)
val add_to_group : user:user -> group:user -> unit Lwt.t
val remove_from_group: user:user -> group:user -> unit Lwt.t

(** Multiple operations on groups *)
val add_to_groups : user:user -> groups:user list -> unit Lwt.t
val add_list_to_group : l:user list -> group:user -> unit Lwt.t

val remove_list_from_group : l:user list -> group:user -> unit Lwt.t



val in_group :
  sp:Eliom_sessions.server_params ->
  ?user:user -> group:user -> unit -> bool Lwt.t



(** Informations on the loggued user *)

val get_user_data : sp:Eliom_sessions.server_params -> userdata Lwt.t
val get_user_id : sp:Eliom_sessions.server_params -> userid Lwt.t
val get_user_name : sp:Eliom_sessions.server_params -> string Lwt.t

val is_logged_on : sp:Eliom_sessions.server_params -> bool Lwt.t


val set_session_data : sp:Eliom_sessions.server_params -> userid * string -> unit Lwt.t


val user_from_userlogin_xform: string -> user Xform.convert Lwt.t




module GenericRights : sig
  (** Helper functions and definitions to define
      [User_sql.Types.admin_writer_reader] objects *)

  type admin_writer_reader_access =
      { field : 'a. 'a admin_writer_reader -> 'a parameterized_group }

  val grp_admin: admin_writer_reader_access
  val grp_write: admin_writer_reader_access
  val grp_read:  admin_writer_reader_access

  val map_awr: (admin_writer_reader_access -> 'a) -> ('a * 'a * 'a)
  val map_awr_lwt:
    (admin_writer_reader_access -> 'a Lwt.t) -> ('a * 'a * 'a) Lwt.t
  val iter_awr_lwt: (admin_writer_reader_access -> unit Lwt.t) -> unit Lwt.t


  val create_admin_writer_reader:
    prefix:string -> name:string -> descr:string -> find_param:find_param ->
    'a admin_writer_reader

  val admin_writer_reader_groups:
    'a admin_writer_reader ->
    ('a Opaque.int32_t -> user) *
    ('a Opaque.int32_t -> user) *
    ('a Opaque.int32_t -> user)


end
