(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
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

module Flag = Flag_parsing_ml
module Ast = Ast_ml
module T = Parser_ml
module TH   = Token_helpers_ml
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

type program_and_tokens = 
  Ast_ml.program option * Parser_ml.token list

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
    try 
      let rec tokens_aux acc = 
        let tok = Lexer_ml.token lexbuf in
        if !Flag.debug_lexer 
        then Common.pr2_gen tok;

        let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
           match ii.PI.token with
           | PI.OriginTok pi ->
               PI.OriginTok (PI.complete_token_location_large file table pi)
           | _ -> raise Todo
        })
        in
        if TH.is_eof tok
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
      in
      tokens_aux []
  with
  | Lexer_ml.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                 (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )
          
let tokens a = 
  Common.profile_code "Parse_ml.tokens" (fun () -> tokens2 a)

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

      if TH.is_comment v (* || other condition to pass tokens ? *)
      then lexer_function (*~pass*) tr lexbuf
      else v

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

exception Parse_error of Parse_info.info

let parse2 filename = 

  let stat = Parse_info.default_stat filename in
  let toks = tokens filename in
  let tr = Parse_info.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  try 
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Common.profile_code "Parser_ml.main" (fun () ->
        if filename =~ ".*\\.mli"
        then Parser_ml.interface      (lexer_function tr) lexbuf_fake
        else Parser_ml.implementation (lexer_function tr) lexbuf_fake
      )
    in
    stat.PI.correct <- (Common.cat filename +> List.length);
    (Some xs, toks), stat
      
  (*| Semantic_c.Semantic _  *)
  with (Lexer_ml.Lexical _ | Parsing.Parse_error) as exn   ->

    let cur = tr.PI.current in
    if not !Flag.error_recovery
    then raise (Parse_error (TH.info_of_tok cur));

    if !Flag.show_parsing_error
    then begin
      (match exn with
      (* Lexical is not anymore launched I think *)
      | Lexer_ml.Lexical s -> 
          pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
      | Parsing.Parse_error -> 
          pr2 ("parse error \n = " ^ error_msg_tok cur)
      (* | Semantic_java.Semantic (s, i) -> 
         pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
      *)
      | _e -> raise Impossible
      );

      let filelines = Common2.cat_array filename in
      let checkpoint2 = Common.cat filename +> List.length in
      let line_error = TH.line_of_tok cur in
      Parse_info.print_bad line_error (0, checkpoint2) filelines;
    end;

    stat.PI.bad     <- Common.cat filename +> List.length;
    (None, toks), stat

let parse a = 
  Common.profile_code "Parse_ml.parse" (fun () -> parse2 a)

let parse_program file = 
  let ((astopt, _toks), _stat) = parse file in
  Common2.some astopt

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(* This is similar to what I did for OPA. This is also similar
 * to what I do for parsing hacks for C++, but this fuzzy AST can be useful
 * on its own, e.g. for a not too bad sgrep/spatch.
 *)
let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
