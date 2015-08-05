(* Yoann Padioleau
 * 
 * Copyright (C) 2012 Facebook
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

module Ast = Ast_opa
module Flag = Flag_parsing_opa
module T = Parser_opa
module TH   = Token_helpers_opa
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Lots of copy paste with my other parsers (e.g. C++, PHP, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_opa.program * Parser_opa.token list

(*****************************************************************************)
(* Tokens/Ast association  *)
(*****************************************************************************)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    Lexer_opa.reset();
    
    try 
      let opa_token lexbuf = 
        (* old: Lexer_opa.token lexbuf *)
        if !Flag.debug_lexer then Common.pr2_gen (Lexer_opa.current_mode());
        match Lexer_opa.current_mode () with
        | Lexer_opa.ST_INITIAL -> Lexer_opa.initial lexbuf
        | Lexer_opa.ST_DOUBLE_QUOTES -> Lexer_opa.in_double_quote lexbuf
        | Lexer_opa.ST_IN_XML_TAG tag -> Lexer_opa.in_xml_tag tag lexbuf
        | Lexer_opa.ST_IN_XML_TEXT tag -> Lexer_opa.in_xml_text tag lexbuf
        | Lexer_opa.ST_IN_CSS -> Lexer_opa.in_css lexbuf
        | Lexer_opa.ST_IN_PARSER -> Lexer_opa.in_parser lexbuf
      in
      
      let rec tokens_aux acc = 
        let tok = opa_token lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;
        if not (TH.is_comment tok)
        then Lexer_opa._last_non_whitespace_like_token := Some tok;

        let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
           match ii.PI.token with
           | PI.OriginTok pi ->
               PI.OriginTok 
                 (PI.complete_token_location_large file table pi)
           | _ -> raise Todo
        })
        in
        
        if TH.is_eof tok
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
      in
      tokens_aux []
  with
  | Lexer_opa.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                 (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )
let tokens a = 
  Common.profile_code "Parse_opa.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(* Hacked lex. Ocamlyacc expects a function returning one token at a time
 * but we actually lex all the file so we need a wrapper to turn that
 * into a stream.
 * This function use refs passed by parse. 'tr' means 'token refs'. 
 *)
let rec lexer_function tr = fun lexbuf ->
  match tr.PI.rest with
  | [] -> (pr2 "LEXER: ALREADY AT END"; tr.PI.current)
  | v::xs -> 
      tr.PI.rest <- xs;
      tr.PI.current <- v;
      tr.PI.passed <- v::tr.PI.passed;
      if TH.is_comment v
      then lexer_function (*~pass*) tr lexbuf
      else v


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

exception Parse_error of Parse_info.info

let parse2 filename = 

  let toks = tokens filename in

  let tr = Parse_info.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  (* -------------------------------------------------- *)
  (* Call parser *)
  (* -------------------------------------------------- *)
  try 
    (Common.profile_code "Parser_opa.main" (fun () ->
      Parser_opa.main (lexer_function tr) lexbuf_fake, toks
    ))
  with exn ->
    let current = tr.PI.current in

    if not !Flag.error_recovery 
    then raise (Parse_error (TH.info_of_tok current));

    if !Flag.show_parsing_error
    then 
      (match exn with
      (* Lexical is not anymore launched I think *)
      | Lexer_opa.Lexical s -> 
          pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok current)
      | Parsing.Parse_error -> 
          pr2 ("parse error \n = " ^ error_msg_tok current)
            (* | Semantic_java.Semantic (s, i) -> 
               pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
            *)
      | _e -> raise Impossible
      );
    [], toks

let parse a = 
  Common.profile_code "Parse_opa.parse" (fun () -> parse2 a)

let parse_just_tokens file =
  let toks = tokens file in
  [], toks
