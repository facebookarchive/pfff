(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo: extract and factorize more from comment_php.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo: duplicate of matcher/parse_fuzzy.ml *)
type 'tok hooks = {
  kind: 'tok -> Parse_info.token_kind;
  tokf: 'tok -> Parse_info.info;
}

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)


let comment_before hooks tok all_toks =
  let pos = Parse_info.pos_of_info tok in
  let before = 
    all_toks +> Common2.take_while (fun tok2 ->
      let info = hooks.tokf tok2 in
      let pos2 = PI.pos_of_info info in
      pos2 < pos
    )
  in
  let first_non_space =
    List.rev before +> Common2.drop_while (fun t ->
      let kind = hooks.kind t in
      match kind with
      | PI.Esthet PI.Newline | PI.Esthet PI.Space -> true
    | _ -> false
    )
  in
  match first_non_space with
  | x::_xs when hooks.kind x =*= PI.Esthet (PI.Comment) ->
      let info = hooks.tokf x in
      if PI.col_of_info info = 0
      then Some info
      else None
  | _ -> None


(*
let comment_after tok all_toks =
  let pos = PI.pos_of_info tok in
  let after = 
    all_toks +> Common2.drop_while (fun tok2 ->
      let info = TH.info_of_tok tok2 in
      let pos2 = PI.pos_of_info info in
      pos2 <= pos
    )
  in
  let first_non_space =
    after +> Common2.drop_while (function
    | Parser_php.TNewline _ | Parser_php.TSpaces _ -> true
    | _ -> false
    )
  in
  match first_non_space with
  | (Parser_php.T_COMMENT ii | Parser_php.T_DOC_COMMENT ii)::_xs ->
      Some ii
  | _ -> None

*)
