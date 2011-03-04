(* Ocsimore
 * Copyright (C) 2005
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
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
   @author Boris Yakobowski
*)

open User_sql.Types

open Forum_types

(** create a new forum. [?arborescent] is true by default. 
    Setting it to false will prevent to comment comments. *)
val new_forum : 
  title:string -> 
  descr:string -> 
  ?arborescent:bool ->
  title_syntax: 'res Wiki_types.content_type ->
  messages_wiki:Wiki_types.wiki ->
  comments_wiki:Wiki_types.wiki ->
  unit ->
  forum Lwt.t

(** inserts a message in a forum. 
    [?moderated] and [?sticky] are false by default. *)
val new_message :
  sp:Eliom_sessions.server_params ->
  forum:forum ->
  wiki:Wiki_types.wiki ->
  creator_id:userid ->
  title_syntax:'res Wiki_types.content_type ->
  ?subject:string ->
  ?parent_id:message ->
  ?moderated:bool ->
  ?sticky:bool ->
  text:string ->
  message Lwt.t

(** set ou unset sticky flag on a message *)
val set_sticky :
  message_id:message -> sticky:bool -> unit Lwt.t
  
(** set or unset moderated flag on a message *)
val set_moderated :
  message_id:message -> moderated:bool -> unit Lwt.t
  
(** Get forum information, given its id or title.
    Information is: (forum id, title, description, arborescent, deleted)
*)
val get_forum: 
  ?not_deleted_only:bool ->
  ?forum:forum -> 
  ?title:string -> 
  unit -> 
  forum_info Lwt.t

(** returns the list of forums *)
val get_forums_list : ?not_deleted_only:bool -> unit ->
  raw_forum_info list Lwt.t
  
(** returns a message *)
val get_message : 
  message_id:message -> 
  unit ->
  message_info Lwt.t
  
(** returns a list of messages containing the message of id [~message_id]
    and all its children, ordered according depth first traversal of the tree.
    For each message, the information retrieved is:
    [(id, subject, author, datetime, parent_id, root_id, forum_id, wikibox, 
    moderated, sticky, tree_min, tree_max)]. 
    The list is not filtered and also contains deleted messages.
    The result is ordered according to tree_min.
*)
val get_thread : 
  message_id:message -> 
  unit ->
  raw_message_info list Lwt.t
  

(** returns the list of messages (without comments) in a forum.
    If [moderated_only] is true, will return:
    - moderated messages
    - all messages with special rights (without looking at rights)
*)
val get_message_list : 
  forum:Forum_types.forum ->
  first:int64 ->
  number:int64 ->
  moderated_only:bool ->
  unit ->
  raw_message_info list Lwt.t
  
(** returns the creator of a wikibox containing a forum message 
    or forum message title. [None] if not a forum wikibox. *)
val get_wikibox_creator : 
  wb:Wiki_types.wikibox -> User_sql.Types.userid option Lwt.t

(** returns whether the message has been moderated or not.
    The argument is either the wikibox containing the message or the
    message title. The result is [false] if it is not a forum wikibox.
*)
val wikibox_is_moderated : wb:Wiki_types.wikibox -> bool Lwt.t
