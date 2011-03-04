(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_charset_mime.mli Copyright (C) 2008
 * Boris Yakobowski
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type extension = string
type file = string
type filename = string


(** Charset *)

(** By convention, "no specified charset" is represented by the empty
    string *)
type charset = string
val no_charset: charset

(** Association between extensions and charset, with a default value. *)
type charset_assoc

(** All files are mapped to [no_charset] *)
val empty_charset_assoc : ?default:charset -> unit -> charset_assoc

val find_charset : string -> charset_assoc -> charset

(** Functions related to the default charset in the association *)
val default_charset : charset_assoc -> charset
val set_default_charset : charset_assoc -> charset -> charset_assoc


(** Updates the mapping between extensions from a file to its charset.
    The update can be specified using the extension of the file,
    the name of the file, or the entire file (with its path)
*)
val update_charset_ext : charset_assoc -> extension -> charset -> charset_assoc
val update_charset_file : charset_assoc -> filename -> charset -> charset_assoc
val update_charset_regexp :
  charset_assoc -> Netstring_pcre.regexp -> charset -> charset_assoc



(** MIME types; the default value is ["application/octet-stream"] *)
type mime_type = string

val default_mime_type : mime_type

(** association between extensions and mime types, with default value *)
type mime_assoc

(** Default values, obtained by reading the file specified by
    [Ocsigen_config.get_mimefile] *)
val default_mime_assoc : unit -> mime_assoc


(** Parsing of a file containing mime associations, such as /etc/mime-types *)
val parse_mime_types : filename:string -> mime_assoc


(* The other functions are as for charsets *)

val find_mime : file -> mime_assoc -> string

val default_mime : mime_assoc -> mime_type
val set_default_mime : mime_assoc -> mime_type -> mime_assoc

val update_mime_ext : mime_assoc -> extension -> mime_type -> mime_assoc
val update_mime_file : mime_assoc -> filename -> mime_type -> mime_assoc
val update_mime_regexp :
  mime_assoc -> Netstring_pcre.regexp -> mime_type -> mime_assoc







