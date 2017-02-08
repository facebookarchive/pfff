(*
 * pa_optcomp.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of optcomp.
 *)

(** Optionnal compilation with cpp-like directives *)

open Camlp4.PreCast

(** Representation of values supported by optcomp. *)
type value =
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Tuple of value list

module Env : Map.S with type key = string
  (** Mapping from strings. *)

type env = value Env.t
  (** Type of environments. *)

val env : env ref
  (** The global environment. *)

val define : string -> value -> unit
  (** [define id value] binds [id] to [value] in the global
      environment. *)

val eval : env -> Ast.expr -> value
  (** [eval env expr] tries to evalute [expr] in [env]. It raises
      [Failure] if the expression cannot be statically evaluated. *)

val string_of_value_o : value -> string
  (** Returns a string representation of the given value, in original
      syntax. *)

val string_of_value_r : value -> string
  (** Returns a string representation of the given value, in revised
      syntax. *)

val default_lexer : string -> in_channel -> (Token.t * Loc.t) Stream.t
  (** The default lexer. It returns a filtered token stream. *)

val filter : ?lexer : (string -> in_channel -> (Token.t * Loc.t) Stream.t) -> Token.Filter.token_filter
  (** The optcomp stream filter. [lexer] is the lexer to use for
      included files. Its default value is {!default_lexer}. *)

val get_quotation_value : string -> value
  (** [get_quotation_value id] returns the value registered for the
      given quotation.

      After filtering a token stream, optcomp quotations contents are
      replaced by identifiers, this function returns the value
      assocaited to a given quotation identifier. *)

val expr_of_value : Loc.t -> value -> Ast.expr
  (** [expr_of_value loc value] converts a value to an expression. *)

val patt_of_value : Loc.t -> value -> Ast.patt
  (** [patt_of_value loc value] converts a value to a pattern. *)
