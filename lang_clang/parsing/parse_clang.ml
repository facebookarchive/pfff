(* Yoann Padioleau
 * 
 * Copyright (C) 2013 Facebook
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

module Flag = Flag_parsing_clang
module PI = Parse_info

open Ast_clang
open Parser_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. C++, PHP, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    try 
      let a_token lexbuf = 
        Lexer_clang.token lexbuf
      in
      
      let rec tokens_aux acc = 
        let tok = a_token lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

        if (match tok with Parser_clang.EOF  -> true | _ -> false)
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
      in
      tokens_aux []
  with
  | Lexer_clang.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                   (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )

let tokens a = 
  Common.profile_code "Parse_clang.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec sexp_list acc ending toks =
  match toks with
  | x::xs when x =*= ending -> List.rev acc, xs

  | TOPar::TUpperIdent s::THexInt _dontcare::xs ->
      let (body, xs) = sexp_list [] TCPar xs in
      sexp_list (Paren (s, body)::acc) ending xs

  | TOAngle::xs ->
      let (body, xs) = sexp_list [] TCAngle xs in
      sexp_list (Anchor body::acc) ending xs
  | TOBracket::xs ->
      let (body, xs) = sexp_list [] TCBracket xs in
      sexp_list (Bracket body::acc) ending xs
  | TInf::xs ->
      let (body, xs) = sexp_list [] TSup xs in
      sexp_list (Angle body::acc) ending xs

  | t::xs -> sexp_list (T t::acc) ending xs
  | [] -> 
      failwith ("unterminated sexp_list: " ^
                   (match ending with
                   | TCPar -> "')'"
                   | TCAngle -> "'>>'"
                   | TSup -> "'>'"
                   | TCBracket -> "']'"
                   | _ -> raise Impossible
                   ))

      

let parse file =
  let toks = tokens file in
  let (body, rest) = sexp_list [] EOF toks in
  (match body, rest with
  | [Paren (s,args)], [] -> Paren (s, args)
  | _ -> 
      failwith "noise after sexp"
  )

