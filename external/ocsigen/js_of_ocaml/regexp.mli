(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Raphaël Proust, Jérôme Vouillon
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

(** Types for regexps *)

type regexp
(** The type for regexps. *)

type result
(** The type for match result. *)

(** Constructors *)

val regexp: string -> regexp
(** Simple regexp constructor. *)

val quote: string -> string
(** Escapes characters with special meaning in the regexp context. *)

val regexp_string: string -> regexp
(** [regexp_string s] creates a regexp matching the exact string [s]. *)

(** Functions *)

val string_match: regexp -> string -> int -> result option
(** [string_match r s i] matches the string [s] starting from the [i]th
    character. Evaluates to [None] if [s] (from the [i]th character) doesn't
    match [r]. *)

val search: regexp -> string -> int -> (int * result) option
(** [search r s i] evaluates to the index of the match and the match result or
    [None] if [s] (starting from [i]) doesn't match [r]. *)

val matched_string : result -> string
(** [matched_string r] return the exact substring that matched when evaluating
    [r]. *)

val matched_group : result -> int -> string option
(** [matched_group r i] is the [i]th group matched. Groups in matches are
  * obtained with parentheses. Groups are 1-based. *)

val global_replace: regexp -> string -> string -> string
(** [global_replace r s by] replaces all of the matches of [r] in [s] by [by]. *)

val replace_first: regexp -> string -> string -> string
(** [replace_first r s by] replaces the first match of [r] in [s] by [by]. *)

val split: regexp -> string -> string list
(** [split r s] splits the string [s] erasing matches with [r].
    [split (regexp " ") "toto tutu tata"] is [["toto";"tutu";"tata"]].*)

val bounded_split: regexp -> string -> int -> string list
(** [bounded_split r s i] is like [split r s] except that the result's length is
    less than [i]. *)
