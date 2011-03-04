(* Ocsimore
 * Copyright (C) 2008
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
   @author Boris Yakobowski
   @author Vincent Balat
*)

open User_sql.Types


(** Semi-abstract type for a wiki *)
type wiki_arg = [ `Wiki ]
type wiki = wiki_arg Opaque.int32_t

(** Conversions from a wiki index *)
val string_of_wiki : wiki -> string
val wiki_of_string : string -> wiki
val sql_of_wiki : wiki -> int32
val wiki_of_sql : int32 -> wiki

type wikibox_arg = [ `Wikibox ]
type wikibox = wikibox_arg Opaque.int32_t

val sql_of_wikibox : wikibox -> int32
val wikibox_of_sql : int32 -> wikibox
val string_of_wikibox : wikibox -> string

type wikipage = wiki * string
type wikipage_arg = [ `Wikipage ]
type wikipage_uid = wikipage_arg Opaque.int32_t

type wiki_model
type 'a content_type (** The parameter is the type of the content,
                         once translated to xhtml
                         (usually flows or inlines) *)
val string_of_wiki_model : wiki_model -> string
val wiki_model_of_string : string -> wiki_model
val string_of_content_type : 'a content_type -> string
val content_type_of_string : string -> 'a content_type

(** Fields for a wiki *)
type wiki_info = {
  wiki_id : wiki;
  wiki_title : string;
  wiki_descr : string;
  wiki_pages : string option;
  wiki_boxrights : bool;
  wiki_container : wikibox option;
  wiki_staticdir : string option (** if static dir is given,
                                ocsimore will serve static pages if present,
                                instead of wiki pages *);
  wiki_model : wiki_model;
  wiki_siteid: string option;
}

type wikipage_info = {
  wikipage_wiki: wiki;
  wikipage_wikibox: wikibox;
  wikipage_page: string;
  wikipage_title: string option;
  wikipage_uid : wikipage_uid;
}

type wikibox_info = {
  wikibox_wiki : wiki;
  wikibox_comment: string option;
  wikibox_special_rights: bool;
  wikibox_id : wikibox;
}

type media_type_elem =
    [ `All
    | `Aural
    | `Braille
    | `Embossed
    | `Handheld
    | `Print
    | `Projection
    | `Screen
    | `Speech
    | `TTY
    | `TV ]
type media_type = media_type_elem list (*Xhtmltypes.mediadesc*)
val media_type_elem_of_string : string -> media_type_elem option
val string_of_media_type_elem : media_type_elem -> string
val media_type_of_string : string -> media_type
val string_of_media_type : media_type -> string

type 'a rights_aux = sp:Eliom_sessions.server_params -> 'a -> bool Lwt.t

class type wiki_rights =
object
  method can_create_wiki : unit rights_aux

  method can_admin_wiki :          wiki rights_aux
  method can_set_wiki_permissions :wiki rights_aux
  method can_create_wikiboxes :    wiki rights_aux
  method can_create_subwikiboxes : wiki rights_aux
  method can_create_wikicss :      wiki rights_aux
  method can_create_wikipages :    wiki rights_aux
  method can_delete_wikiboxes :    wiki rights_aux
  method can_view_static_files :   wiki rights_aux
  method can_edit_metadata :       wiki rights_aux

  method can_admin_wikibox : wikibox rights_aux
  method can_set_wikibox_specific_permissions : wikibox rights_aux
  method can_write_wikibox : wikibox rights_aux
  method can_read_wikibox : wikibox rights_aux
  method can_view_src : wikibox rights_aux
  method can_view_history : wikibox rights_aux
  method can_view_oldversions : wikibox rights_aux
  method can_view_oldversions_src : wikibox rights_aux

  method can_create_wikipagecss : wikipage rights_aux
  method can_admin_wikipage : wikipage rights_aux
end


(** Content of a wikibox. The second field is the actual content. It is [None]
    if the wikibox has been deleted. The third field is the version id *)
type 'a wikibox_content = 'a content_type * string option * int32

(**/**)
val wikibox_data_of_raw :
  (string * userid * string option * CalendarLib.Calendar.t *
     string * int32) option Lwt.t ->
  (string * userid * string option * CalendarLib.Calendar.t *
     'a content_type * int32) option Lwt.t

val raw_of_wikibox_data :
  (string * userid * string option * CalendarLib.Calendar.t *
     'a content_type * int32) option Lwt.t
    ->
  (string * userid * string option * CalendarLib.Calendar.t *
     string * int32) option Lwt.t
