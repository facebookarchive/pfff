(* Ocsimore
 * Copyright (C) 2009
 * Laboratoire PPS - Université Paris Diderot - CNRS
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
(**
   @author Vincent Balat
   @author Boris Yakobowski
*)





(** {2 Database access with verification of permissions} *)

(** create a new forum. [?arborescent] is true by default. 
    Setting it to false will prevent to comment comments.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val new_forum : 
  sp:Eliom_sessions.server_params ->
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  title_syntax: Xhtmltypes_duce.inlines Wiki_types.content_type ->
  messages_wiki:Wiki_types.wiki ->
  comments_wiki:Wiki_types.wiki ->
  unit ->
  Forum_types.forum Lwt.t

(** inserts a message in a forum. 
    [?moderated] and [?sticky] are false by default.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val new_message :
  sp:Eliom_sessions.server_params ->
  forum:Forum_types.forum ->
  creator_id:User_sql.Types.userid ->
  ?subject:string ->
  ?parent_id:Forum_types.message ->
  ?sticky:bool ->
  text:string ->
  unit ->
  Forum_types.message Lwt.t

(** set ou unset sticky flag on a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val set_sticky :
  sp:Eliom_sessions.server_params ->
  message_id:Forum_types.message -> sticky:bool -> unit Lwt.t
  
(** set or unset moderated flag on a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val set_moderated :
  sp:Eliom_sessions.server_params ->
  message_id:Forum_types.message -> moderated:bool -> unit Lwt.t
  
(** Get forum information, given its id or title.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val get_forum: 
  sp:Eliom_sessions.server_params ->
  ?forum:Forum_types.forum -> 
  ?title:string -> 
  unit -> 
  Forum_types.forum_info Lwt.t

(** returns the list of forums visible to the user. *)
val get_forums_list : 
  sp:Eliom_sessions.server_params ->
  unit ->
  Forum_types.forum_info list Lwt.t
  
(** returns id, subject, author, datetime, parent id, root id, forum id, text,
    and moderated, deleted, sticky status of a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val get_message : 
  sp:Eliom_sessions.server_params ->
  message_id:Forum_types.message -> 
  Forum_types.message_info Lwt.t
  
(** returns a list of messages containing the message of id [~message_id]
    and all its children, ordered according depth first traversal of the tree.
    Deleted messages are returned.
    Only readable messages comments are returned.
    Comments of unreadable messages are not returned.
    May fail with exceptions [Ocsimore_common.Permission_denied]
    or [Not_found].
*)
val get_thread : 
  sp:Eliom_sessions.server_params ->
  message_id:Forum_types.message -> 
  Forum_types.message_info list Lwt.t
  
type raw_message

(** returns the list of messages (without comments) in a forum. 
    If the user cannot read unmoderated messages, 
    only moderated messages and messages with special rights
    (must be filtered afterwards!!!) are returned.
*)
val get_message_list : 
  sp:Eliom_sessions.server_params ->
  forum:Forum_types.forum ->
  first:int64 ->
  number:int64 ->
  unit ->
  raw_message list Lwt.t
  
(** translate [raw_message] to [Forum_types.forum_info],
    verifying special rights if needed (only for first messages). 
    Raises [Ocsimore_common.Permission_denied] if special rights do not
    allow to read the message.
*)
val message_info_of_raw_message : 
  sp:Eliom_sessions.server_params ->
  raw_message -> 
  Forum_types.message_info Lwt.t 
