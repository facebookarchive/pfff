(* $Id: netstring_pcre.mli 800 2004-07-09 00:17:47Z stolpmann $ *)

(** Wrapper for regexps with PCRE syntax
 *
 * This module is a version of [Str] with a thread-safe interface,
 * implemented using Pcre. 
 *
 * This modules processes PCRE-style regular expressions. If you like
 * to write [Str]-like regexps, you can also use {!Netstring_str} with
 * almost the same signature.
 *)

(** Supported regexp syntax: See pcrepattern(3). *)

type regexp = Pcre.regexp;;
  (** The type of regular expressions; now based on [Pcre] *)

type split_result = Pcre.split_result = 
  | Text of string
  | Delim of string
  | Group of int * string
  | NoGroup
  (** Here we keep compatiblity with [Pcre] *)
;;

type result;;
  (** The type of matching results *)

val regexp: string -> regexp
  (** Parses a regexp *)
val regexp_case_fold: string -> regexp
  (** Parses a case-insensitive regexp *)
val quote: string -> string
  (** Quotes a string such that it can be included in a regexp *)
val regexp_string: string -> regexp
  (** Returns a regexp that matches exactly the string *)
val regexp_string_case_fold: string -> regexp
  (** Returns a case-insensitive regexp that matches exactly the string *)

(** Note: the [groups] argument is ignored in the following functions.
 * Once upon a time this argument determined how many groups were 
 * copied to the [result] value.
 * Now all groups are accessible in the [result] value, no matter
 * what [groups] says.
 *)

val string_match: 
      ?groups:int -> regexp -> string -> int -> result option
  (** Matches the string at the position with the regexp. Returns
   * [None] if no match is found. Returns [Some r] on success,
   * and [r] describes the match.
   *)

val search_forward: 
      ?groups:int -> regexp -> string -> int -> (int * result)
  (** Searches a match of the string with the regexp, starting at
   * the position and in forward direction.
   * Raises [Not_found] if no match could be found.
   * Returns [(p,r)] when a match at position [p] is found,
   * described by [r].
   *)

val search_backward:
      ?groups:int -> regexp -> string -> int -> (int * result)
  (** Searches a match of the string with the regexp, starting at
   * the position and in backward direction.
   * Raises [Not_found] if no match could be found.
   * Returns [(p,r)] when a match at position [p] is found,
   * described by [r].
   *)

(*
  string_partial_match: not available
*)

(* The ~groups option is ignored *)

val matched_string : result -> string -> string
  (** Extracts the matched part from the string. The string argument
   * must be the same string passed to [string_match] or the search
   * functions, and the result argument must be the corresponding
   * result.
   *)

val match_beginning : result -> int
  (** Returns the position where the matched part begins *)

val match_end : result -> int
  (** Returns the position where the matched part ends *)

val matched_group : result -> int -> string -> string
  (** Extracts the substring the nth group matches from the whole
   * string. The string argument
   * must be the same string passed to [string_match] or the search
   * functions, and the result argument must be the corresponding
   * result.
   *)

val group_beginning : result -> int -> int
  (** Returns the position where the substring matching the nth
   * group begins 
   *)

val group_end : result -> int -> int
  (** Returns the position where the substring matching the nth
   * group ends 
   *)


val global_replace: regexp -> string -> string -> string
  (** [global_replace re templ s]: Replaces all matchings of [re] in
   * [s] by [templ].
   *
   * In [templ] one can refer to matched groups by the backslash notation:
   * [\1] refers to the first group, [\2] to the second etc.
   * [\0] is the whole match. [\\ ] is the backslash character.
   *)

val replace_first: regexp -> string -> string -> string
  (** [replace_first re templ s]: Replaces the first match of [re] in
   * [s] by [templ].
   *
   * In [templ] one can refer to matched groups by the backslash notation:
   * [\1] refers to the first group, [\2] to the second etc.
   * [\0] is the whole match. [\\ ] is the backslash character.
   *)

val global_substitute:
       ?groups:int -> 
       regexp -> (result -> string -> string) -> string -> string
  (** [global_substitute re subst s]: Applies the substitution function
   * [subst] to all matchings of [re] in [s], and returns the 
   * transformed string. [subst] is called with the current [result]
   * of the match and the whole string [s].
   *)

val substitute_first:
       ?groups:int ->
       regexp -> (result -> string -> string) -> string -> string
  (** [substitute_first re subst s]: Applies the substitution function
   * [subst] to the first matching of [re] in [s], and returns the 
   * transformed string. [subst] is called with the current [result]
   * of the match and the whole string [s].
   *)


(* replace_matched: not available *)

val split: regexp -> string -> string list
  (** Splits the string according to the regexp in substrings.
   * Occurrences of the delimiter at the beginning and the end
   * are ignored.
   *)

val bounded_split: regexp -> string -> int -> string list
  (** Splits into at most [n] substrings, based on [split] *)
val split_delim: regexp -> string -> string list
  (** Same as [split], but occurrences of the delimiter at the beginning 
   * and the end are returned as empty strings
   *)
val bounded_split_delim: regexp -> string -> int -> string list
  (** Splits into at most [n] substrings, based on [split_delim] *)
val full_split: regexp -> string -> split_result list
  (** Like [split_delim], but returns the delimiters in the result *)
val bounded_full_split: regexp -> string -> int -> split_result list
  (** Splits into at most [n] substrings, based on [full_split] *)

val string_before: string -> int -> string
  (** The first [n] characters of a string *)
val string_after: string -> int -> string
  (** The last [n] characters of a string *)
val first_chars: string -> int -> string
  (** Same as [string_before] *)
val last_chars: string -> int -> string
  (** Same as [string_after] *)

