(* Ocsimore
 * Copyright (C) 2008
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
   Wiki AST to OcamlDuce
   @author Vincent Balat
*)

open Wiki_types


(** The type for a function acting as a syntax extension *)
type ('res, 'a_content) syntax_extension =
  (Wiki_widgets_interface.box_info, 'res, 'a_content) Wikicreole.plugin


(** The abstract type of the objects able to parse wiki creole syntax,
    possibly with extensions. Those objects are passed as arguments
    to all displaing functions *)
type ('res, 'inline, 'a_content) wikicreole_parser


(** Add a syntax extension to an existing syntax parser.
    If [wiki_content] is [true], it means that your extension
    may contain wikisyntax after "|" (that will be preparsed).
*)
val add_extension :
  wp:('res, 'inline, 'a_content) wikicreole_parser ->
  name:string ->
  ?wiki_content:bool ->
  ('res, 'a_content) syntax_extension ->
  unit


(** The default syntax parser. It parses wiki creole syntax, as well
    as div, span, wikiname, raw, content, menu and cond tags.
    Default (and full) wiki parser.
*)
val wikicreole_parser :
  (Xhtmltypes.div_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t
  ) wikicreole_parser
(* Currently modified in Wiki_widgets and User_widgets *)

(** The same, without subwikiboxes and containers (content).
    Used for example for forum messages.
*)
val reduced_wikicreole_parser0 :
  (Xhtmltypes.div_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t
  ) wikicreole_parser

(** The same, without images, objects, subwikiboxes and containers (content).
    Used for example for forum messages with restricted features.
*)
val reduced_wikicreole_parser1 :
  (Xhtmltypes.div_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t
  ) wikicreole_parser

(** The same, without images, objects, titles, tables, lists,
    subwikiboxes and containers (content). *)
val reduced_wikicreole_parser2 :
  (Xhtmltypes.div_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t
  ) wikicreole_parser

(** For button content. *)
val reduced_wikicreole_parser_button_content :
  (Xhtmltypes.button_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.button_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.button_content XHTML.M.elt list Lwt.t
  ) wikicreole_parser

(** Parser for inline wikicreole. *)
val inline_wikicreole_parser :
  (Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t
  ) wikicreole_parser


(** the content type for wikicreole boxes: *)
val wikicreole_content_type : Xhtmltypes.div_content XHTML.M.elt list Wiki_types.content_type

(** the content type for reduced_wikicreole_parser0: *)
val reduced_wikicreole_content_type0 : Xhtmltypes.div_content XHTML.M.elt list Wiki_types.content_type

(** the content type for reduced_wikicreole_parser1: *)
val reduced_wikicreole_content_type1 : Xhtmltypes.div_content XHTML.M.elt list Wiki_types.content_type

(** the content type for reduced_wikicreole_parser2: *)
val reduced_wikicreole_content_type2 : Xhtmltypes.div_content XHTML.M.elt list Wiki_types.content_type

(** the content type for raw text boxes: *)
val rawtext_content_type : Xhtmltypes.div_content XHTML.M.elt list Wiki_types.content_type

(** the content type for wikicreole inline content.
    It is using [inline_wikicreole_parser]. *)
val wikicreole_inline_content_type :
  Xhtmltypes.inlinemix XHTML.M.elt list Wiki_types.content_type

(** Return a copy of a parser. The calls to [add_extension] on one of the
    copy will not be visible on the other *)
val copy_parser :
  ('res, 'inline, 'a_content) wikicreole_parser ->
  ('res, 'inline, 'a_content) wikicreole_parser



(** Functions called to transform some wikicreole text *)
val add_preparser_extension :
  wp:('res, 'inline, 'a_content) wikicreole_parser ->
  name:string ->
  (Eliom_sessions.server_params * Wiki_types.wikibox,
    string option Lwt.t)
  Wikicreole.plugin_args ->
  unit

val preparse_extension :
  ('res, 'inline, 'a_content) wikicreole_parser ->
  (Eliom_sessions.server_params * Wiki_types.wikibox) ->
  string -> string Lwt.t


(** Sets the extension which will be called on links *)
val set_link_extension :
  wp:('res, 'inline, 'a_content) wikicreole_parser ->
  (string ->
   string option ->
   Wikicreole.attribs ->
   Eliom_sessions.server_params * Wiki_types.wikibox ->
   string option Lwt.t) ->
  unit


(** **)

(** Functions displaying wikicreole code *)


(** Returns the XHTML corresponding to a wiki page *)
val xml_of_wiki :
  ('res_pre list Lwt.t,
   'inline,
   [> `PCDATA ] XHTML.M.elt list Lwt.t
  ) wikicreole_parser ->
  Wiki_widgets_interface.box_info ->
  string ->
  'res_pre list Lwt.t

(** returns only the content of the first paragraph of a wiki text. *)
val inline_of_wiki :
  Wiki_widgets_interface.box_info ->
  string ->
  Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t

(** returns only the content of the first paragraph of a wiki text,
    after having removed links. *)
val a_content_of_wiki :
  Wiki_widgets_interface.box_info ->
  string ->
  Xhtmltypes.a_content XHTML.M.elt list Lwt.t

(** Returns the wiki syntax for an extension box
    from its name, arguments and content.
*)
val string_of_extension :
  string -> (string * string) list -> string option -> string

(** parses common attributes ([class], [id]) *)
val parse_common_attribs :
  (string * string) list -> Xhtmltypes.core XHTML.M.attrib list

(** returns the type of URL.
    [Page] means a page in current wiki ([wiki:page], or [page]),
    [Wiki_page] means a page in another wiki ([wiki(num):page]),
    [Site] means an URL relative to the root of the site ([site:href]),
    [Absolute] means an absolute URL ([<otherscheme>:href]).
*)
type force_https = bool option

type link_kind =
  | Absolute of string
  | Page of string * force_https
  | Wiki_page of Wiki_types.wiki * string * force_https
  | Site of string * force_https

val link_kind : string -> link_kind

val make_href :
  Wiki_widgets_interface.box_info ->
  link_kind -> string option -> string


(** The class to use to denote the fact that the content comes
    from the specified wikibox *)
val class_wikibox: wikibox -> string



val translate_link :
  oldwiki:wiki ->
  newwiki:wiki ->
  newwikipath:string ->
  string ->
  string option ->
  Wikicreole.attribs ->
  Eliom_sessions.server_params * wikibox ->
  string option Lwt.t
