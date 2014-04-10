(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

module Flag = Flag_parsing_js
module Ast = Ast_js
module TH   = Token_helpers_js
module T = Parser_js
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. PHP, C, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_js.program option * Parser_js.token list

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

    Lexer_js.reset();
    try 
      let jstoken lexbuf = 
        match Lexer_js.current_mode() with
        | Lexer_js.ST_IN_CODE ->
            Lexer_js.initial lexbuf
        | Lexer_js.ST_IN_XHP_TAG current_tag ->
            Lexer_js.st_in_xhp_tag current_tag lexbuf
        | Lexer_js.ST_IN_XHP_TEXT current_tag ->
            Lexer_js.st_in_xhp_text current_tag lexbuf
        | Lexer_js.ST_IN_BACKQUOTE ->
            Lexer_js.backquote lexbuf
      in
      let rec tokens_aux acc = 
        let tok = jstoken lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

        if not (TH.is_comment tok)
        then Lexer_js._last_non_whitespace_like_token := Some tok;

        let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token =
          (* could assert pinfo.filename = file ? *)
            match ii.PI.token with
            | PI.OriginTok pi ->
              PI.OriginTok (PI.complete_token_location_large file table pi)
            | PI.FakeTokStr _
            | PI.Ab  
            | PI.ExpandedTok _
              -> raise Impossible
        })
        in

        if TH.is_eof tok
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
    in
    tokens_aux []
  with
  | Lexer_js.Lexical s -> 
    failwith ("lexical error " ^ s ^ "\n =" ^ 
                 (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )

let tokens a = 
  Common.profile_code "Parse_js.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(* Hacked lex. This function use refs passed by parse.
 * 'tr' means 'token refs'.
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

  let stat = PI.default_stat filename in
  let toks = tokens filename in
  let toks = Parsing_hacks_js.fix_tokens toks in
  let tr = PI.mk_tokens_state toks in
  let checkpoint = TH.line_of_tok tr.PI.current in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  try
     (* -------------------------------------------------- *)
     (* Call parser *)
      (* -------------------------------------------------- *)
    let xs =
      Common.profile_code "Parser_js.main" (fun () ->
        Parser_js.main (lexer_function tr) lexbuf_fake
      )
    in
    stat.PI.correct <- (Common.cat filename +> List.length);
    (Some xs, toks), stat
  with (Lexer_js.Lexical _ | Parsing.Parse_error) as exn ->
    let cur = tr.PI.current in
    if not !Flag.error_recovery
    then raise (Parse_error (TH.info_of_tok cur));

    if !Flag.show_parsing_error
    then begin
        (match exn with
        (* Lexical is not anymore launched I think *)
        | Lexer_js.Lexical s -> 
            pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
        | Parsing.Parse_error -> 
            pr2 ("parse error \n = " ^ error_msg_tok cur)
        | _e -> raise Impossible
        );
      let filelines = Common2.cat_array filename in
      let checkpoint2 = Common.cat filename +> List.length in
      let line_error = TH.line_of_tok cur in
      PI.print_bad line_error (checkpoint, checkpoint2) filelines;
    end;

    stat.PI.bad     <- Common.cat filename +> List.length;
    (None, toks), stat

let parse a = 
  Common.profile_code "Parse_js.parse" (fun () -> parse2 a)

let parse_program file = 
  let ((astopt, _toks), _stat) = parse file in
  Common2.some astopt


let parse_string (w : string) : Ast.program =
  Common2.with_tmp_file ~str:w ~ext:"js" parse_program

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let (program_of_string: string -> Ast_js.program) = fun s -> 
  Common2.with_tmp_file ~str:s ~ext:"js" (fun file ->
    parse_program file
  )

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
