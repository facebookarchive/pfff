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
   Parser for Wikicreole
   @author Jérôme Vouillon
   @author Vincent Balat
   @author Boris Yakobowski
*)


type attribs = (string * string) list

type ('a, 'b, 'c) ext_kind =
  | Block of 'a
  | A_content of 'b
  | Link_plugin of 'c


(** Arguments for the extension mechanisme, after '<<' *)
type ('param, 'a) plugin_args =
    'param ->
    attribs -> (** Xml-like attributes for the extension (eg val='foo') *)
    string option -> (** content for the extension, after the '|' *)
    'a

type ('param, 'flow, 'a_content) plugin =
    ('param,
     ('flow, 'a_content, (string * attribs * 'a_content)) ext_kind
    ) plugin_args


type ('flow, 'inline, 'a_content, 'param) builder =
  { chars : string -> 'a_content;
    strong_elem : attribs -> 'inline list -> 'a_content;
    em_elem : attribs -> 'inline list -> 'a_content;
    br_elem : attribs -> 'a_content;
    img_elem : attribs -> string -> string -> 'a_content;
    tt_elem : attribs -> 'inline list -> 'a_content;
    monospace_elem : attribs -> 'inline list -> 'a_content;
    underlined_elem : attribs -> 'inline list -> 'a_content;
    linethrough_elem : attribs -> 'inline list -> 'a_content;
    subscripted_elem : attribs -> 'inline list -> 'a_content;
    superscripted_elem : attribs -> 'inline list -> 'a_content;
    nbsp : 'a_content;
    endash : 'a_content;
    emdash : 'a_content;
    a_elem : attribs -> string -> 'a_content list -> 'inline;
    make_href : 'param -> string -> string option -> string;
    (** the string option is the fragment part of the URL (#...)*)
    p_elem : attribs -> 'inline list -> 'flow;
    pre_elem : attribs -> string list -> 'flow;
    h1_elem : attribs -> 'inline list -> 'flow;
    h2_elem : attribs -> 'inline list -> 'flow;
    h3_elem : attribs -> 'inline list -> 'flow;
    h4_elem : attribs -> 'inline list -> 'flow;
    h5_elem : attribs -> 'inline list -> 'flow;
    h6_elem : attribs -> 'inline list -> 'flow;
    ul_elem : attribs -> ('inline list * 'flow option * attribs) list -> 'flow;
    ol_elem : attribs -> ('inline list * 'flow option * attribs) list -> 'flow;
    dl_elem : attribs -> (bool * 'inline list * attribs) list -> 'flow;
    hr_elem : attribs -> 'flow;
    table_elem : attribs ->
      ((bool * attribs * 'inline list) list * attribs) list -> 'flow;
    inline : 'a_content -> 'inline;
(** The syntax of plugins is [<<name arg1='value1' ... argn="valuen' >>] or
[<<name arg1='value1' ... argn="valuen' |content>> ] *)
(** Must display sthg (error message?) if the name does not exist. *)
    plugin : string -> bool * ('param, 'flow, 'a_content) plugin;
    plugin_action :  string -> int -> int -> ('param, unit) plugin_args;
    link_action : string -> string option -> attribs -> int * int -> 'param -> unit;
    error : string -> 'a_content;
  }

val from_channel :
  'param ->
  ('flow, 'inline, 'a_content, 'param) builder -> in_channel -> 'flow list Lwt.t

val from_string :
  'param ->
  ('flow, 'inline, 'a_content, 'param) builder -> string -> 'flow list Lwt.t

val from_lexbuf :
  'param ->
  ('flow, 'inline, 'a_content, 'param) builder -> Lexing.lexbuf -> 'flow list Lwt.t

