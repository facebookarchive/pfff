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

open User_sql.Types

  (** Semi-abstract type for a forum *)
  type forum_arg = [ `Forum ]
  type forum = forum_arg Opaque.int32_t

  (** Semi-abstract type for a message or comment *)
  type message_arg = [ `Message ]
  type message = message_arg Opaque.int32_t

  val sql_of_message_option : message option -> int32 option
  val message_of_sql_option : int32 option -> message option
  val sql_of_message : message -> int32
  val message_of_sql : int32 -> message
  val sql_of_forum : forum -> int32
  val forum_of_sql : int32 -> forum
  val sql_of_forum_option : forum option -> int32 option
  val forum_of_sql_option : int32 option -> forum option
  val string_of_forum : forum -> string
  val forum_of_string : string -> forum
  val string_of_message : message -> string
  val message_of_string : string -> message

  type forum_info = {
    f_id: forum;
    f_title: string;
    f_descr: string;
    f_arborescent: bool;
    f_deleted: bool;
    f_title_syntax: Xhtmltypes_duce.inlines Wiki_types.content_type;
    f_messages_wiki: Wiki_types.wiki;
    f_comments_wiki: Wiki_types.wiki;
  }

  type message_info = {
    m_id: message;
    m_creator_id: User_sql.Types.userid;
    m_datetime: CalendarLib.Calendar.t;
    m_parent_id: message option;
    m_root_id: message;
    m_forum: forum;
    m_subject: Wiki_types.wikibox option;
    m_wikibox: Wiki_types.wikibox;
    m_moderated: bool;
    m_sticky: bool;
    m_has_special_rights: bool Lwt.t Lazy.t;
    m_tree_min: int32;
    m_tree_max: int32;
  }

  type raw_forum_info = (int32 * string * string * bool * bool * string * int32 * int32)
  type raw_message_info =
      (int32 * int32 * CalendarLib.Calendar.t * int32 option *
         int32 * int32 * int32 option * int32 * bool * bool * bool
       * int32 * int32)

  val get_forum_info : raw_forum_info -> forum_info
  val get_message_info : raw_message_info -> message_info

