(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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
(* A few helpers function to build Ast_fuzzy tree from a list of tokens.
 * It factorizes the language-independent part of those ast fuzzy builder
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo: factorize code with lib_unparser.esthet type to have
 * for each language a token_kind_of_tok. We then can avoid all those
 * is_eof, is_comment, elt_of_tok, and hooks.kind in all the fuzzy parsers.
 *)
type token_kind =
  | LPar
  | RPar
  | LBrace
  | RBrace
  | Other

type 'tok hooks = {
  kind: 'tok -> token_kind;
  tokf: 'tok -> Parse_info.info;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
 * Assumes work on a list of tokens without comments.
 * 
 * less: I should also factorize with Parse_cpp.parse_fuzzy. 
 * put here also generic parts of  token_views_of_xxx?
 * 
 * less: check that it's consistent with the indentation? 
 * less: more fault tolerance? if col == 0 and { then reset?
 *)

let mk_trees h xs =

  let rec consume x xs =
    match x with
    | tok when h.kind tok = LBrace -> 
        let body, closing, rest = look_close_brace x [] xs in
        Ast_fuzzy.Braces (h.tokf x, body, h.tokf closing), rest
    | tok when h.kind tok = LPar ->
        let body, closing, rest = look_close_paren x [] xs in
        let body' = split_comma body in
        Ast_fuzzy.Parens (h.tokf x, body', h.tokf closing), rest
    | tok -> 
      Ast_fuzzy.Tok (PI.str_of_info (h.tokf tok), h.tokf x), xs
(*
    (match Ast.str_of_info (tokext tok) with
    | "..." -> Ast_fuzzy.Dots (tokext tok)
    | s when Ast_fuzzy.is_metavar s -> Ast_fuzzy.Metavar (s, tokext tok)
    | s -> Ast_fuzzy.Tok (s, tokext tok)
*)
  
  and aux xs =
  match xs with
  | [] -> []
  | x::xs ->
      let x', xs' = consume x xs in
      x'::aux xs'

  and look_close_brace tok_start accbody xs =
    match xs with
    | [] -> 
        failwith (spf "PB look_close_brace (started at %d)" 
                    (PI.line_of_info (h.tokf tok_start)))
    | x::xs -> 
        (match x with
        | tok when h.kind tok = RBrace-> 
          List.rev accbody, x, xs

        | _ -> let (x', xs') = consume x xs in
               look_close_brace tok_start (x'::accbody) xs'
        )

  and look_close_paren tok_start accbody xs =
    match xs with
    | [] -> 
        failwith (spf "PB look_close_paren (started at %d)" 
                     (PI.line_of_info (h.tokf tok_start)))
    | x::xs -> 
        (match x with
        | tok when h.kind tok = RPar -> 
            List.rev accbody, x, xs
        | _ -> 
            let (x', xs') = consume x xs in
            look_close_paren tok_start (x'::accbody) xs'
        )

  and split_comma xs =
     let rec aux acc xs =
       match xs with
       | [] ->
         if null acc
         then []
         else [Left (acc +> List.rev)]
       | x::xs ->
         (match x with
         | Ast_fuzzy.Tok (",", info) ->
           let before = acc +> List.rev in
           if null before
           then aux [] xs
           else (Left before)::(Right (info))::aux [] xs
         | _ ->
           aux (x::acc) xs
         )
     in
     aux [] xs
  in
  aux xs
