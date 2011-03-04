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

(** {2 Forum related groups} *)
val message_creators : Wiki_types.wiki_arg parameterized_group
val message_creators_notmod : Wiki_types.wiki_arg parameterized_group
val message_moderators : Wiki_types.wiki_arg parameterized_group
val message_deletors : Wiki_types.wiki_arg parameterized_group
val message_deletors_if_creator : Wiki_types.wiki_arg parameterized_group
val message_modifiers : Wiki_types.wiki_arg parameterized_group
val message_modifiers_if_creator : Wiki_types.wiki_arg parameterized_group
val message_sticky_makers : Wiki_types.wiki_arg parameterized_group
val moderated_message_readers  : Wiki_types.wiki_arg parameterized_group
val message_readers_evennotmoderated  : Wiki_types.wiki_arg parameterized_group

val creators : forum_arg parameterized_group
val creators_notmod : forum_arg parameterized_group
val moderators : forum_arg parameterized_group
val deletors : forum_arg parameterized_group
val deletors_if_creator : forum_arg parameterized_group
val modifiers : forum_arg parameterized_group
val modifiers_if_creator : forum_arg parameterized_group
val sticky_makers : forum_arg parameterized_group
val moderated_readers  : forum_arg parameterized_group
val readers  : forum_arg parameterized_group
val forum_admin : forum_arg parameterized_group
val forum_visible : forum_arg parameterized_group

val forum_creators : user

val thread_comments_creators  : message_arg parameterized_group
val thread_comments_creators_notmod  : message_arg parameterized_group
val thread_moderated_readers  : message_arg parameterized_group
val thread_readers_evennotmoderated  : message_arg parameterized_group

(** {2 Forum creation} *)

(** Creates a new forum or returns its id without modification
    if it already exists. *)
val create_forum : 
  wiki_model:Wiki_types.wiki_model ->
  title_syntax:Xhtmltypes_duce.inlines Wiki_types.content_type ->
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  unit ->
  Forum_types.forum_info Lwt.t

(** {2 Session data} *)

type role = 
    {
      message_creators : bool Lwt.t Lazy.t;
      message_creators_notmod : bool Lwt.t Lazy.t;
      message_moderators : bool Lwt.t Lazy.t;
      message_deletors : bool Lwt.t Lazy.t;
      message_deletors_if_creator : bool Lwt.t Lazy.t;
      message_modifiers : bool Lwt.t Lazy.t;
      message_modifiers_if_creator : bool Lwt.t Lazy.t;
      message_sticky_makers : bool Lwt.t Lazy.t;
      moderated_message_readers : bool Lwt.t Lazy.t;
      message_readers_evennotmoderated : bool Lwt.t Lazy.t;

      comment_creators : bool Lwt.t Lazy.t;
      comment_creators_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_creator : bool Lwt.t Lazy.t;
      comment_modifiers : bool Lwt.t Lazy.t;
      comment_modifiers_if_creator : bool Lwt.t Lazy.t;
      comment_sticky_makers : bool Lwt.t Lazy.t;
      moderated_comment_readers : bool Lwt.t Lazy.t;
      comment_readers_evennotmoderated : bool Lwt.t Lazy.t;

      creators : bool Lwt.t Lazy.t;
      creators_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_creator : bool Lwt.t Lazy.t;
      modifiers : bool Lwt.t Lazy.t;
      modifiers_if_creator : bool Lwt.t Lazy.t;
      sticky_makers : bool Lwt.t Lazy.t;
      moderated_readers : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }


val get_role : 
  sp:Eliom_sessions.server_params ->
  Forum_types.forum -> role Lwt.t



(** {2 } *)
type forum_action_info =
  | Preview of ((Forum_types.forum * 
                   Forum_types.message option (* parent *)) * string)
  | Msg_creation_not_allowed of (Forum_types.forum * 
                                   Forum_types.message option (* parent *))


(** {2 Eliom related values} *)

(** Eliom parameter type for forums *)
val eliom_forum :
  string -> (forum, [`WithoutSuffix], [`One of forum] Eliom_parameters.param_name) Eliom_parameters.params_type

(** Eliom parameter type for messages *)
val eliom_message :
  string -> (message, [`WithoutSuffix], [`One of message] Eliom_parameters.param_name) Eliom_parameters.params_type

(** Eliom input field for forums *)
val eliom_forum_input : 
  ?a:Eliom_duce.Xhtml.input_attrib_t ->
  input_type: Eliom_duce.Xhtml.input_type_t ->
  ?name:[< Forum_types.forum Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
  ?value:Forum_types.forum -> unit -> Eliom_duce.Xhtml.input_elt

(** Eliom input field for messages *)
val eliom_message_input : 
  ?a:Eliom_duce.Xhtml.input_attrib_t ->
  input_type: Eliom_duce.Xhtml.input_type_t ->
  ?name:[< Forum_types.message Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
  ?value:Forum_types.message -> unit -> Eliom_duce.Xhtml.input_elt

(** Eliom button for messages *)
val eliom_message_button :
  ?a:Eliom_duce.Xhtml.button_attrib_t ->
  name:[< Forum_types.message Eliom_parameters.setone ] Eliom_parameters.param_name ->
  value:Forum_types.message ->
  {{ [ Xhtmltypes_duce.button_content* ] }} ->
  Xhtmltypes_duce.button


(** {2 } *)

class wiki_rights : Wiki_types.wiki_rights

