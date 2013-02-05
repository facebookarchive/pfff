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
  Lexer_clang.line := 1;
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
  line_open_tok: int;
  check_topar: bool;
}

let rec sexp_list env acc ending toks =
  match toks with
  | x::xs when x =*= ending -> List.rev acc, xs

  (* the hex address seems actually used when one wants to crossref
   * information in the AST, e.g. implicit param references.
   *)
  | TOPar l::TUpperIdent (("ImplicitCastExpr") as s)::THexInt _dontcare::xs ->
      let newenv = {line_open_tok = l; check_topar = false} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (s, body)::acc) ending xs

  | TOPar l::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {line_open_tok = l; check_topar = true} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (s, body)::acc) ending xs


  | TOPar l::TLowerIdent "super"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__Super__" ^ s, body)::acc) ending xs

  | TOPar l::TLowerIdent (("public" | "protected" | "virtual") as s)::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (spf "__%s__" s, body)::acc) ending xs

  | TOPar l::TUpperIdent "TemplateArgument"::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__TemplateArgument__", body)::acc) ending xs

  | TOPar l::TLowerIdent "instance"::TCPar::xs ->
      sexp_list env (Paren ("__Instance__", [])::acc) ending xs

  | TOPar l::TLowerIdent "original"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__Original__" ^ s, body)::acc) ending xs


  | TOPar l::TLowerIdent "cleanup"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__Cleanup__" ^ s, body)::acc) ending xs

  | TOPar l::TLowerIdent "capture"::TLowerIdent "byref"::TUpperIdent s::
      THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__CaptureByRef__" ^ s, body)::acc) ending xs
  | TOPar l::TLowerIdent "capture"::TLowerIdent "nested"::TUpperIdent s::
      THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__CaptureNested__" ^ s, body)::acc) ending xs

  | TOPar l::TLowerIdent "capture"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__Capture__" ^ s, body)::acc) ending xs


  | TOPar l::TLowerIdent (("getter" | "setter") as s1)::TUpperIdent s2
    ::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren (spf "__%s__" s1 ^ s2, body)::acc) ending xs

  | TOPar l::TInf _::TInf _::TInf _::TUpperIdent "NULL"
    ::TSup::TSup::TSup::TCPar::xs->
      sexp_list env (Paren ("__Null__", [])::acc) ending xs

  | TOPar l::TDots::TCPar::xs ->
      sexp_list env (Paren ("__Dots__", [])::acc) ending xs

  | TOPar l::TLowerIdent "class"::TCPar::xs ->
      sexp_list env (Paren ("__Class__", [])::acc) ending xs

  | TOPar l::TUpperIdent "ADL"::TCPar::xs ->
      sexp_list env (Paren ("__ADL__", [])::acc) ending xs
  | TOPar l::TLowerIdent "no"::TUpperIdent "ADL"::TCPar::xs ->
      sexp_list env (Paren ("__NoADL__", [])::acc) ending xs

  | TOPar l::TUpperIdent "CXXCtorInitializer"::TUpperIdent s::THexInt _dontcare::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__CXXCtorInitializer__" ^ s, body)::acc) ending xs

  | TOPar l::TUpperIdent "CXXCtorInitializer"::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__CXXCtorInitializer__", body)::acc) ending xs

(*
  | TInf
    ::TUpperIdent ("UncheckedDerivedToBase"|"DerivedToBase"|"BaseToDerived")
    ::TOPar::_::TCPar::TSup::xs ->
    (* SKIP *)
    sexp_list env (Angle []::acc) ending xs
  | TInf
    ::TUpperIdent ("UncheckedDerivedToBase"|"DerivedToBase"|"BaseToDerived")
    ::TOPar::_::TArrow::_::TCPar::TSup::xs ->
    (* SKIP *)
    sexp_list env (Angle []::acc) ending xs
  | TInf
    ::TUpperIdent ("UncheckedDerivedToBase"|"DerivedToBase"|"BaseToDerived")
    ::TOPar::_::TArrow::_::TArrow::_::TCPar::TSup::xs ->
    (* SKIP *)
    sexp_list env (Angle []::acc) ending xs
*)



  | TOBracket l::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCBracket xs in
      sexp_list env (Bracket body::acc) ending xs
  | TInf l::xs ->
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TSup xs in
      sexp_list env (Angle body::acc) ending xs


  | TOPar l::TUpperIdent _::xs ->
    if env.check_topar 
    then failwith (spf "open paren without hexint at line %d" l)
    else
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__SKIPPED__", body)::acc) ending xs
      
  | TOPar l::xs ->
    if env.check_topar 
    then failwith (spf "open paren without constructor at line %d" l)
    else
      let newenv = {env with line_open_tok = l} in 
      let (body, xs) = sexp_list newenv  [] TCPar xs in
      sexp_list env (Paren ("__SKIPPED__", body)::acc) ending xs

  | t::xs -> sexp_list env (T t::acc) ending xs
  | [] -> 
      failwith (spf "unterminated sexp_list '%s' opened at line %d"
                   (match ending with
                   | TCPar -> "')'"
                   | TSup -> "'>'"
                   | TCBracket -> "']'"
                   | _ -> raise Impossible
                   ) env.line_open_tok)

let parse file =
  let toks = tokens file in
  let env = { line_open_tok = 0; check_topar = true } in
  let (body, _rest) = sexp_list env [] EOF toks in
  (match body with
  | [Paren (s,args)]-> Paren (s, args)
  | [Paren (s,args);T Error] -> 
      pr2 (spf "PB with %s" file);
      Paren (s, args)
  | _ -> 
      failwith "noise after sexp"
  )

