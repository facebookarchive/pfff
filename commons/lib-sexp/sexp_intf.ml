(* File: sexp_intf.ml

    Copyright (C) 2005-

      Jane Street Holding, LLC
      Author: Markus Mottl
      email: mmottl\@janestcapital.com
      WWW: http://www.janestcapital.com/ocaml

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** Sexp_intf: interface specification for handling S-expressions (I/O, etc.) *)

open Format
open Lexing

module type S = sig

  (** Type of S-expressions *)
  type t = Type.t = Atom of string | List of t list


  (** {6 Defaults} *)

  val default_indent : int ref
  (** [default_indent] reference to default indentation level for
      human-readable conversions.  Initialisation value: 2. *)


  (** {6 S-expression size} *)

  val size : t -> int * int
  (** [size sexp] @return [(n_atoms, n_chars)], where [n_atoms] is
      the number of atoms in S-expression [sexp], and [n_chars] is the
      number of characters in the atoms of the S-expression. *)


  (** {6 Scan functions} *)

  val scan_sexp : ?buf : Buffer.t -> lexbuf -> t
  (** [scan_sexp ?buf lexbuf] scans an S-expression from lex buffer
      [lexbuf] using the optional string buffer [buf] for storing
      intermediate strings. *)

  val scan_sexps : ?buf : Buffer.t -> lexbuf -> t list
  (** [scan_sexps ?buf lexbuf] reads a list of whitespace separated
      S-expressions from lex buffer [lexbuf] using the optional string
      buffer [buf] for storing intermediate strings. *)

  val scan_iter_sexps : ?buf : Buffer.t -> f : (t -> unit) -> lexbuf -> unit
  (** [scan_iter_sexps ?buf ~f lexbuf] iterates over all whitespace
      separated S-expressions scanned from lex buffer [lexbuf] using
      function [f], and the optional string buffer [buf] for storing
      intermediate strings. *)

  val scan_fold_sexps :
    ?buf : Buffer.t -> f : (t -> 'a -> 'a) -> init : 'a -> lexbuf -> 'a
  (** [scan_fold_sexps ?buf ~f ~init lexbuf] folds over all whitespace
      separated S-expressions scanned from lex buffer [lexbuf] using
      function [f], initial state [init], and the optional string buffer
      [buf] for storing intermediate strings. *)

  val scan_cnv_sexps : ?buf : Buffer.t -> f : (t -> 'a) -> lexbuf -> 'a list
  (** [scan_cnv_sexps ?buf ~f lexbuf] maps all whitespace separated
      S-expressions scanned from lex buffer [lexbuf] to some list using
      function [f], and the optional string buffer [buf] for storing
      intermediate strings. *)


  (** {6 (Partial) parsing} *)

  (** Position information after complete parse *)
  type parse_pos = Pre_sexp.parse_pos =
    private
      {
        mutable text_line : int;  (** Line position in parsed text *)
        mutable text_char : int;  (** Character position in parsed text *)
        mutable buf_pos : int;  (** Reading position in text buffer *)
      }

  (** Type of result from calling {!Sexp.parse}. *)
  type 'a parse_result = 'a Pre_sexp.parse_result =
    | Done of t * parse_pos  (** [Done (sexp, parse_pos)] finished
                                 parsing an S-expression.  Current parse
                                 position is [parse_pos]. *)
    | Cont of bool * 'a parse_fun  (** [Cont (ws_only, parse_fun)] met the
                                       end of input before completely
                                       parsing an S-expression.  The user
                                       has to call [parse_fun] to continue
                                       parsing the S-expression in another
                                       buffer.  If [ws_only] is true, only
                                       whitespace has been parsed so far (or
                                       comments!).  NOTE: the continuation
                                       may only be called once! *)

  and 'a parse_fun = pos : int -> len : int -> 'a -> 'a parse_result
  (** Type of parsing functions with given offsets and lengths. *)

  (** Type of state maintained during parsing *)
  type parse_state = Pre_sexp.parse_state =
    private
      {
        parse_pos : parse_pos;  (** Current parse position *)
        mutable pstack : t list list;  (** Stack of found S-expression lists *)
        pbuf : Buffer.t;  (** Current atom buffer *)
      }

  (** Type of parse errors *)
  type parse_error = Pre_sexp.parse_error =
    {
      location : string;  (** Function in which the parse failed *)
      err_msg : string;  (** Reason why parsing failed *)
      parse_state : parse_state;  (** State of parser *)
    }

  (** Exception raised during partial parsing *)
  exception ParseError of parse_error

  val parse :
    ?text_line : int -> ?text_char : int -> ?pos : int -> ?len : int -> string
    -> string parse_result
  (** [parse ?text_line ?text_char ?pos ?len str] (partially) parses an
      S-expression in string buffer [str] starting at position [pos]
      and reading at most [len] characters.  The text position can be
      initialized with [text_line] and [text_char].  To parse a single
      atom that is not delimited by whitespace it is necessary to call
      this function a second time with the returned continuation, and a
      dummy buffer that contains whitespace.

      @param text_line default = 1
      @param text_char default = 1
      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  open Bigarray

  type bstr = (char, int8_unsigned_elt, c_layout) Array1.t

  val parse_bstr :
    ?text_line : int -> ?text_char : int -> ?pos : int -> ?len : int -> bstr
    -> bstr parse_result
  (** [parse_bstr ?text_line ?text_char ?pos ?len str] same as [parse],
      but operates on bigstrings. *)

  val input_sexp :
    ?text_line : int -> ?text_char : int -> ?buf_pos : int -> in_channel -> t
  (** [input_sexp ?text_line ?text_char ?buf_pos ic] parses an S-expression
      from input channel [ic] using initial position information
      [text_line], [text_char], and [buf_pos].  NOTE: this function is not
      as fast on files as {!Sexp.load_sexp}, and is also slightly slower
      than the scan-functions.  But it is guaranteed that [input_sexp]
      is only going to read data parseable as an S-expression.  Thus,
      subsequent input functions will see the data immediately following it.

      @param text_line default = [1]
      @param text_char default = [1]
      @param buf_pos default = [0]
  *)

  val input_sexps :
    ?text_line : int -> ?text_char : int -> ?buf_pos : int ->
    ?buf : string -> in_channel -> t list
  (** [input_sexps ?text_line ?text_char ?buf_pos ??buf ic] parses
      whitespace separated S-expressions from input channel [ic] until
      EOF is reached.  Faster than the scan-functions.  NOTE: [buf_pos]
      is the initial global buffer position used for locating errors and
      does not refer to [buf].

      @param text_line default = [1]
      @param text_char default = [1]
      @param buf_pos default = [0]
  *)

  val input_rev_sexps :
    ?text_line : int -> ?text_char : int -> ?buf_pos : int ->
    ?buf : string -> in_channel -> t list
  (** [input_rev_sexps ?buf ic] same as {!Sexp.input_sexps}, but returns a
      reversed list of S-expressions, which is slightly more efficient. *)


  (** {6 Loading} *)

  val load_sexp : ?buf : string -> string -> t
  (** [load_sexp ?buf file] reads one S-expression from file [file] using
      buffer [buf] for storing intermediate data.  Ignores any trailing
      data.  Faster than the scan-functions.

      @raise ParseError if the S-expression is unparseable.
      @raise End_of_file if no S-expression could be read.
  *)

  val load_sexps : ?buf : string -> string -> t list
  (** [load_sexps file] reads a list of whitespace separated S-expressions
      from file [file] using buffer [buf] for storing intermediate data.
      Faster than the scan-functions.

      @raise ParseError if there is unparseable data in the file.
      @raise End_of_file if the last S-expression is incomplete.
  *)

  val load_rev_sexps : ?buf : string -> string -> t list
  (** [load_rev_sexps file] same as {!Sexp.load_sexps}, but returns a
      reversed list of S-expressions, which is slightly more efficient. *)


  (** {6 Output of S-expressions to I/O-channels} *)

  val output_hum : out_channel -> t -> unit
  (** [output_hum oc sexp] outputs S-expression [sexp] to output channel
      [oc] in human readable form. *)

  val output_hum_indent : int -> out_channel -> t -> unit
  (** [output_hum_indent indent oc sexp] outputs S-expression [sexp]
      to output channel [oc] in human readable form using indentation level
      [indent].
  *)

  val output_mach : out_channel -> t -> unit
  (** [output_mach oc sexp] outputs S-expression [sexp] to output channel
      [oc] in machine readable (i.e. most compact) form. *)

  val output : out_channel -> t -> unit
  (** [output oc sexp] same as [output_mach]. *)


  (** {6 Output of S-expressions to formatters} *)

  val pp_hum : formatter -> t -> unit
  (** [pp_hum ppf sexp] outputs S-expression [sexp] to formatter [ppf]
      in human readable form. *)

  val pp_hum_indent : int -> formatter -> t -> unit
  (** [pp_hum_indent n ppf sexp] outputs S-expression [sexp] to formatter
      [ppf] in human readable form and indentation level [n]. *)

  val pp_mach : formatter -> t -> unit
  (** [pp_mach ppf sexp] outputs S-expression [sexp] to formatter [ppf]
      in machine readable (i.e. most compact) form. *)

  val pp : formatter -> t -> unit
  (** [pp ppf sexp] same as [pp_mach]. *)


  (** {6 String and bigstring conversions} *)

  val of_string : string -> t
  (** [of_string str] converts string [str] to an S-expression. *)

  val of_bstr : bstr -> t
  (** [of_bstr bstr] converts bigstring [bstr] to an S-expression. *)

  val to_string_hum : ?indent : int -> t -> string
  (** [to_string_hum ?indent sexp] converts S-expression [sexp] to a
      string in human readable form with indentation level [indent].

      @param indent default = [!default_indent]
  *)

  val to_string_mach : t -> string
  (** [to_string_mach sexp] converts S-expression [sexp] to a string in
      machine readable (i.e. most compact) form. *)

  val to_string : t -> string
  (** [to_string sexp] same as [to_string_mach]. *)


  (** {6 Buffer conversions} *)

  val to_buffer_hum : buf : Buffer.t -> ?indent : int -> t -> unit
  (** [to_buffer_hum ~buf ?indent sexp] outputs the S-expression [sexp]
      converted to a string in human readable form to buffer [buf].

      @param indent default = [!default_indent]
  *)

  val to_buffer_mach : buf : Buffer.t -> t -> unit
  (** [to_buffer_mach ~buf sexp] outputs the S-expression [sexp] converted
      to a string in machine readable (i.e. most compact) form to buffer [buf].
  *)

  val to_buffer : buf : Buffer.t -> t -> unit
  (** [to_buffer ~buf sexp] same as {!to_buffer_mach}. *)


  (** {6 Utilities for automated type conversions} *)

  val unit : t
  (** [unit] the unit-value as expressed by an S-expression. *)

  external sexp_of_t : t -> t = "%identity"
  (** [sexp_of_t sexp] maps S-expressions which are part of a type with
      automated S-expression conversion to themselves. *)

  external t_of_sexp : t -> t = "%identity"
  (** [t_of_sexp sexp] maps S-expressions which are part of a type with
      automated S-expression conversion to themselves. *)

end
