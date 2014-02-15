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

open Parser_opa
module TH = Token_helpers_opa

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * Parsing OPA turns out to be quite difficult. The official grammar 
 * is not yacc compliant (opalang/ uses a PEG parser, not a LALR(1) one).
 * This module provides a view of tokens, a view where
 * tokens at least are grouped together in a tree: parenthesis
 * chunks, braced chunks, xml chunks, etc.
 * This is similar to what I do in token_views_cpp.ml.
 * 
 * This tree can then be used in highlight_opa.ml or ast_fuzzy_opa.ml to
 * at least detect and colorize appropriately functions, types, etc.
 *)

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)
type token = Parser_opa.token

type tree =
  | T of token
  | Paren of tree list list (* grouped by comma *)
  | Brace of tree list list (* grouped by comma too, as in type defs *)
  | Bracket of tree list list (* should not have comma, but to factorize code *)
  (* TODO *)
  | Xml of tree list (* attributes *) * tree list (* children *)
 (* with tarzan *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
let is_closing_of start x = 
  match start, x with
  | TOParen _, TCParen _
  | TOBrace _, TCBrace _
  | TOBracket _, TCBracket _
      -> true
  | _ -> false

let error tok =
  let info = TH.info_of_tok tok in
  Parse_info.error_message_info info

(*****************************************************************************)
(* View builder  *)
(*****************************************************************************)

let rec mk_tree xs =
  match xs with
  | [] -> []
  | [EOF _] -> []
  | x::xs ->
      (match x with

      | TOParen _ii ->
          let body, xs = mk_comma_group x [] xs in
          (Paren body)::mk_tree xs
      | TOBrace _ii ->
          let body, xs = mk_comma_group x [] xs in
          (Brace body)::mk_tree xs
      | TOBracket _ii ->
          let body, xs = mk_comma_group x [] xs in
          (Bracket body)::mk_tree xs

      (* todo *)
      | T_XML_OPEN_TAG _ii ->
          (T x)::mk_tree xs
          

      | TCParen _ii | TCBrace _ii | TCBracket _ii ->
          failwith ("wrongly parenthised code: " ^ error x)

      (* todo *)
      | T_XML_CLOSE_TAG (_, _ii)
          -> 
          (T x)::mk_tree xs

      | x ->
          (T x)::mk_tree xs
      )
and mk_comma_group start acc_before_sep xs =
  match xs with
  | [] -> failwith ("could not find end of parenthesis: " ^ error start)
  | x::xs ->
      (match x with
      | x when is_closing_of start x ->
          [List.rev acc_before_sep], xs

      | TCParen _ii | TCBrace _ii | TCBracket _ii -> 
          failwith ("wrongly parenthised code: " ^ error x)

      | TComma _ ->
          let body, xs = mk_comma_group start [] xs in
          (List.rev acc_before_sep)::body, xs

      (* recurse. todo? factorize with code above? *)
      | TOParen _ii ->
          let body, xs = mk_comma_group x [] xs in
          mk_comma_group start ((Paren body)::acc_before_sep) xs
      | TOBrace _ii ->
          let body, xs = mk_comma_group x [] xs in
          mk_comma_group start ((Brace body)::acc_before_sep) xs
      | TOBracket _ii ->
          let body, xs = mk_comma_group x [] xs in
          mk_comma_group start ((Bracket body)::acc_before_sep) xs

      (* todo *)
      | T_XML_CLOSE_TAG (_, _ii) ->
          mk_comma_group start ((T x)::acc_before_sep) xs

      | _ ->
          mk_comma_group start ((T x)::acc_before_sep) xs
      )

(*****************************************************************************)
(* Meta  *)
(*****************************************************************************)

let vof_token t =
  Ocaml.VString (Token_helpers_opa.str_of_tok t)

let rec vof_tree =
  function
  | T v1 -> let v1 = vof_token v1 in Ocaml.VSum (("T", [ v1 ]))
  | Paren v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Paren", [ v1 ]))
  | Brace v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Brace", [ v1 ]))
  | Bracket v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Bracket", [ v1 ]))
  | Xml ((v1, v2)) ->
      let v1 = Ocaml.vof_list vof_tree v1
      and v2 = Ocaml.vof_list vof_tree v2
      in Ocaml.VSum (("Xml", [ v1; v2 ]))
  
let vof_tree_list xs = Ocaml.vof_list vof_tree xs
