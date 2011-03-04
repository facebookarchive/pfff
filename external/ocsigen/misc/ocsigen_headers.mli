(* Ocsigen
 * ocsigen_headers.mli Copyright (C) 2005 Vincent Balat
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

(** Getting informations from HTTP header. *)
(** This module uses the lowel level module Ocsigen_http_frame.Http_header.
     It is very basic and must be completed for exhaustiveness. *)


val get_keepalive : Ocsigen_http_frame.Http_header.http_header -> bool
val parse_cookies : string  -> string Ocsigen_lib.String_Table.t
val parse_mime_type : string -> string option * string option
val get_host_from_host_header : Ocsigen_http_frame.t -> 
  string option * int option
val get_user_agent : Ocsigen_http_frame.t -> string
val get_cookie_string : Ocsigen_http_frame.t -> string option
val get_if_modified_since : Ocsigen_http_frame.t -> float option
val get_if_unmodified_since : Ocsigen_http_frame.t -> float option
val get_if_none_match : Ocsigen_http_frame.t -> string list option
val get_if_match : Ocsigen_http_frame.t -> string list option
val get_content_type : Ocsigen_http_frame.t -> string option
val parse_content_type : string option -> ((string * string) * (string * string) list) option
val get_content_length : Ocsigen_http_frame.t -> int64 option
val get_referer : Ocsigen_http_frame.t -> string option
val get_referrer : Ocsigen_http_frame.t -> string option
val get_accept :
  Ocsigen_http_frame.t ->
  ((string option * string option) * float option * (string * string) list)
    list
val get_accept_charset : Ocsigen_http_frame.t -> (string option * float option) list
val get_accept_encoding : Ocsigen_http_frame.t -> (string option * float option) list
val get_accept_language : Ocsigen_http_frame.t -> (string * float option) list
val get_range : Ocsigen_http_frame.t -> ((int64 * int64) list * int64 option * Ocsigen_extensions.ifrange) option
