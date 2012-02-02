(* Yoann Padioleau
 * 
 * Copyright (C) 2012, Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Parser_opa

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * Parsing OPA turns out to be quite difficult. The official grammar 
 * is not yacc compliant (mlstate uses a PEG parser, not a LALR(1) one).
 * This module provides a degenerated AST for OPA, one where
 * things at least are grouped together in a tree: parenthesis
 * chunk, braced chunks, xml tree, etc).
 * This is similar to what I do in token_views_cpp.ml.
 * 
 * This tree can then be used in highlight_opa.ml to at least
 * detect and colorize appropriately functions, types, etc.
 *)

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)
type token = Parser_opa.token

type tree =
  | T of token
  | Paren of tree list list (* grouped by comma *)
  | Brace of tree list list (* grouped by comma too, as in type defs *)
  | Bracket of tree list
  | Xml of tree list (* attributes *) * tree list (* children *)
 (* with tarzan *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(*****************************************************************************)
(* View builder  *)
(*****************************************************************************)

let rec mk_tree xs =
  match xs with
  | [] -> []
  | [EOF _] -> []
  | x::xs ->
      (match x with
      | TOParen ii ->
          raise Todo
      | TOBrace ii ->
          raise Todo
      | TOBracket ii ->
          raise Todo
      | T_XML_OPEN_TAG ii ->
          raise Todo

      | TCParen ii | TCBrace ii | TCBracket ii 
      | T_XML_CLOSE_TAG (_, ii)
          -> 
          failwith ("wrongly parenthised code")
      | x ->
          (T x)::mk_tree xs
      )

(*****************************************************************************)
(* Meta  *)
(*****************************************************************************)

let vof_tree_list xs =
  raise Todo
