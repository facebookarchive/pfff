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

type env = {
  line: int ref;
  line_open_tok: int;
}

let rec sexp_list env acc ending toks =
  match toks with
  | x::xs when x =*= ending -> List.rev acc, xs

  (* the hex address seems actually used when one wants to crossref
   * information in the AST, e.g. implicit param references.
   *)
  | TOPar::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren (s, body)::acc) ending xs

  | TOPar::TLowerIdent "super"::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__Super__" ^ s, body)::acc) ending xs

  | TOPar::TLowerIdent "public"::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__Public__", body)::acc) ending xs

  | TOPar::TLowerIdent "cleanup"::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__Cleanup__" ^ s, body)::acc) ending xs

  | TOPar::TLowerIdent "capture"::TLowerIdent "byref"::TUpperIdent s::
      THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__CaptureByRef__" ^ s, body)::acc) ending xs

  | TOPar::TLowerIdent "capture"::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__Capture__" ^ s, body)::acc) ending xs

  | TOPar::TLowerIdent "getter"::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__Getter__" ^ s, body)::acc) ending xs
  | TOPar::TLowerIdent "setter"::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__Setter__" ^ s, body)::acc) ending xs

  | TOPar::TInf::TInf::TInf::TUpperIdent "NULL"::TSup::TSup::TSup::TCPar::xs ->
      incr env.line;
      sexp_list env (Paren ("__Null__", [])::acc) ending xs

  | TOPar::TDots::TCPar::xs ->
      incr env.line;
      sexp_list env (Paren ("__Dots__", [])::acc) ending xs

  | TOPar::TLowerIdent "instance"::TCPar::xs ->
      sexp_list env (Paren ("__Instance__", [])::acc) ending xs

  | TOPar::TUpperIdent "CXXCtorInitializer"::TUpperIdent s::THexInt _dontcare::xs ->
      incr env.line;
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCPar xs in
      sexp_list env (Paren ("__CXXCtorInitializer__" ^ s, body)::acc) ending xs



  | TOBracket::xs ->
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TCBracket xs in
      sexp_list env (Bracket body::acc) ending xs
  | TInf::xs ->
      let (body, xs) = 
        sexp_list {env with line_open_tok = !(env.line)} [] TSup xs in
      sexp_list env (Angle body::acc) ending xs


  | TOPar::TUpperIdent _::xs ->
      failwith (spf "open paren without hexint at line %d" !(env.line))
  | TOPar::xs ->
      xs +> Common.take_safe 20 +> List.iter pr2_gen;
      failwith (spf "open paren without constructor at line %d" !(env.line))

  | t::xs -> sexp_list env (T t::acc) ending xs
  | [] -> 
      failwith (spf "unterminated sexp_list %s at line %d, opened at line %d"
                   (match ending with
                   | TCPar -> "')'"
                   | TSup -> "'>'"
                   | TCBracket -> "']'"
                   | _ -> raise Impossible
                   ) !(env.line) env.line_open_tok)

let parse file =
  let toks = tokens file in
  let env = { line = ref 1; line_open_tok = 0 } in
  let (body, rest) = sexp_list env [] EOF toks in
  (match body, rest with
  | [Paren (s,args)], [] -> Paren (s, args)
  | _ -> 
      failwith "noise after sexp"
  )

