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

(* $Id$ *)

(** Pure ocaml module for conversion between UCS and UTF8 *)

type unichar = int
type unistring = unichar array

(** [from_unichar 0xiii] converts a code point [iii] (usually in hexadecimal
    form) into a string containing the UTF-8 encoded character [0xiii]. See 
    {{:http://www.unicode.org/}unicode.org} for charmaps.
    Does not check that the given code point is a valid unicode point. *)
val from_unichar : unichar -> string
val from_unistring : unistring -> string

(** [to_unichar_validated] decodes an UTF-8 encoded code point and checks
    for incomplete characters, invalid characters and overlong encodings. 
    @raise Convert.Error if invalid *)
val to_unichar_validated : string -> pos:int ref -> unichar

(** [to_unichar] decodes an UTF-8 encoded code point. Result is undefined 
    if [pos] does not point to a valid UTF-8 encoded character. *)
val to_unichar : string -> pos:int ref -> unichar

(** [to_unistring] decodes an UTF-8 encoded string into an array of
    [unichar]. The string {e must} be valid. *)
val to_unistring : string -> unistring

(** [first_char] returns the first UTF-8 encoded code point. *)
val first_char : string -> unichar

(** [next] returns the position of the code point following the one at [pos]. *)
val next : string -> pos:int -> int

(** [length] returns the number of code-points in the UTF-8 encode string *)
val length : string -> int

(** [to_unichar_validated] may raise [PARTIAL_INPUT] or [ILLEGAL_SEQUENCE] *)
module Error : sig
  type error = 
    | NO_CONVERSION
        (** Conversion between the requested character sets is not supported *)
    | ILLEGAL_SEQUENCE (** Invalid byte sequence in conversion input *)
    | FAILED (** Conversion failed for some reason *)
    | PARTIAL_INPUT (** Partial character sequence at end of input *)
    | BAD_URI (** URI is invalid *)
    | NOT_ABSOLUTE_PATH (** Pathname is not an absolute path *)
  exception Error of error * string

  val raise_bad_utf8 : unit -> 'a
end
