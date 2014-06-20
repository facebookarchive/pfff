(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module Ast = Ast_java
module Flag = Flag_parsing_java
module T = Parser_java
module TH = Token_helpers_java
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
type program_and_tokens = Ast_java.program option * Parser_java.token list

(*****************************************************************************)
(* Error diagnostic *)
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
    let rec tokens_aux () =
      let tok = Lexer_java.token lexbuf in
      if !Flag.debug_lexer then Common.pr2_gen tok;

      (* fill in the line and col information *)
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
      then [tok]
      else tok::(tokens_aux ())
    in
    tokens_aux ()
  with
    | Lexer_java.Lexical s ->
        failwith ("lexical error " ^ s ^ "\n =" ^
                  (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
    | e -> raise e
 )

let tokens a =
  Common.profile_code "Java parsing.tokens" (fun () -> tokens2 a)

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

  let stat = Parse_info.default_stat filename in
  let filelines = Common2.cat_array filename in

  let toks = tokens filename in
  let toks = Parsing_hacks_java.fix_tokens toks in

  let tr = Parse_info.mk_tokens_state toks in
  let checkpoint = TH.line_of_tok tr.PI.current in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  let elems =
    try (
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Left
        (Common.profile_code "Parser_java.main" (fun () ->
          Parser_java.goal (lexer_function tr) lexbuf_fake
        ))
    ) with e ->

      let line_error = TH.line_of_tok tr.PI.current in

      let _passed_before_error = tr.PI.passed in
      let current = tr.PI.current in

      (* no error recovery, the whole file is discarded *)
      tr.PI.passed <- List.rev toks;

      let info_of_bads = Common2.map_eff_rev TH.info_of_tok tr.PI.passed in

      Right (info_of_bads, line_error, current, e)
  in

  match elems with
  | Left xs ->
      stat.PI.correct <- (Common.cat filename +> List.length);
      (Some xs, toks), stat

  | Right (_info_of_bads, line_error, cur, exn) ->

      if not !Flag.error_recovery
      then raise (Parse_error (TH.info_of_tok cur));

      (match exn with
      | Lexer_java.Lexical _ | Parsing.Parse_error (*|Semantic_c.Semantic _  *)
        -> ()
      | e -> raise e
      );

      if !Flag.show_parsing_error
      then
        (match exn with
        (* Lexical is not anymore launched I think *)
        | Lexer_java.Lexical s ->
            pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
        | Parsing.Parse_error ->
            pr2 ("parse error \n = " ^ error_msg_tok cur)
        | _e -> raise Impossible
        );
      let checkpoint2 = Common.cat filename +> List.length in

      if !Flag.show_parsing_error
      then Parse_info.print_bad line_error (checkpoint, checkpoint2) filelines;
      stat.PI.bad     <- Common.cat filename +> List.length;
      (None, toks), stat

let parse a =
  Common.profile_code "Parse_java.parse" (fun () -> parse2 a)

let parse_program file =
  let ((ast, _toks), _stat) = parse file in
  Common2.some ast

let parse_string (w : string)
    : (Ast_java.program option * Parser_java.token list) * Parse_info.parsing_stat =
  Common2.with_tmp_file ~str:w ~ext:"java" parse

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
