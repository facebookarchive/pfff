(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Simple XML lexer *)

(** This module provides an [ocamllex] lexer for XML files. It only
    supports the most basic features of the XML specification. 

    The lexer altogether ignores the following 'events': comments,
    processing instructions, XML prolog and doctype declaration.

    The predefined entities ([&amp;], [&lt;], etc.) are supported. The
    replacement text for other entities whose entity value consist of
    character data can be provided to the lexer (see
    {!Xml_lexer.entities}). Internal entities declarations are {e not}
    taken into account (the lexer just skips the doctype declaration).

    [CDATA] sections and character references are supported.

    See {!Xml_lexer.strip_ws} about whitespace handling.
*)

(** {3 Error reporting} *)

type error =
  | Illegal_character of char
  | Bad_entity of string
  | Unterminated of string
  | Tag_expected
  | Attribute_expected
  | Other of string
val error_string : error -> string

exception Error of error * int
(** This exception is raised in case of an error during the
    parsing. The [int] argument indicates the character position in
    the buffer. Note that some non-conforming XML documents might not
    trigger an error. *)

(** {3 API} *)

(** The type of the XML document elements *)
type token =
  | Tag of string * (string * string) list * bool
	(** [Tag (name, attributes, empty)] denotes an opening tag 
	   with the specified [name] and [attributes]. If [empty], 
	   then the tag ended in "/>", meaning that it has no 
	   sub-elements. *)
  | Chars of string
        (** Some text between the tags *)
  | Endtag of string
        (** A closing tag *)
  | EOF
        (** End of input *)

val strip_ws : bool ref
(** Whitespace handling: if [strip_ws] is [true] (the default), 
    whitespaces next to a tag are ignored. Character data consisting 
    only of whitespaces is thus suppressed (i.e. [Chars ""] tokens are
    skipped). *)

val entities : (string * string) list ref
(** An association list of entities definitions. Initially, it
    contains the predefined entities ([ ["amp", "&"; "lt", "<" ...] ]). *)

val token : Lexing.lexbuf -> token
(** The entry point of the lexer.
    @return the next token in the buffer
    @raise Error in case of an invalid XML document *)
