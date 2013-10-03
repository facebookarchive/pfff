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
  Ast_js.program (* NotParsedCorrectly if parse error *) * Parser_js.token list

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
        Lexer_js.initial lexbuf
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
(* Lexer tricks *)
(*****************************************************************************)

let rparens_of_if toks = 
  let toks = Common.exclude TH.is_comment toks in

  let stack = ref [] in

  let rparens_if = ref [] in

  toks +> Common2.iter_with_previous_opt (fun prev x -> 
    (match x with
    | T.T_LPAREN _ -> 
        Common.push2 prev stack;
    | T.T_RPAREN info ->
        if !stack <> [] then begin
        let top = Common2.pop2 stack in
        (match top with
        | Some (T.T_IF _) -> 
            Common.push2 info rparens_if
        | _ ->
            ()
        )
        end
    | _ -> ()
    )
  );
  !rparens_if

(* UGLYYYYYYYYYYYYYYYYYYY. Better would be to read section 7.6.2.
*)
let rec adjust_tokens xs =

  let rparens_if = rparens_of_if xs in
  let hrparens_if = Common.hashset_of_list rparens_if in

  match xs with
  | [] -> []
  | y::ys ->
      let res = ref [] in
      Common.push2 y res;
      let rec aux prev f xs = 
        match xs with
        | [] -> ()
        | e::l ->
            if TH.is_comment e
            then begin 
              Common.push2 e res;
              aux prev f l
            end else begin
              f prev e;
              aux e f l
            end
      in
      let f = (fun prev x ->
        match prev, x with
        | (T.T_LCURLY _ | T.T_SEMICOLON _ | T.T_VIRTUAL_SEMICOLON _), 
        T.T_RCURLY _ ->
            Common.push2 x res;
            (* also one after ? *)
(*            Common.push2 (T.T_VIRTUAL_SEMICOLON (Ast.fakeInfo ())) res; *)

        | _, T.T_RCURLY _ ->
            let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
            Common.push2 (T.T_VIRTUAL_SEMICOLON fake) res;
            Common.push2 x res;
            (* also one after ? *)
(*            Common.push2 (T.T_VIRTUAL_SEMICOLON (Ast.fakeInfo ())) res; *)
            
        | (T.T_SEMICOLON _ | T.T_VIRTUAL_SEMICOLON _),
            T.EOF _ ->
            Common.push2 x res;
        | _, T.EOF _ ->
            let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
            Common.push2 (T.T_VIRTUAL_SEMICOLON fake) res;
            Common.push2 x res;


        | T.T_RCURLY _, 
            (T.T_IDENTIFIER _ | 
             T.T_IF _ | T.T_VAR _ | T.T_FOR _ | T.T_RETURN _ |
             T.T_SWITCH _ |
             T.T_FUNCTION _ | T.T_THIS _ |
             T.T_BREAK _ | 
             T.T_NEW _

            ) 
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push2 (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push2 x res;

        (* this is valid only if the RPAREN is not the closing paren
         * of a if
         *)
        | T.T_RPAREN info, 
            (T.T_VAR _ | T.T_IF _ | T.T_THIS _ | T.T_FOR _ | T.T_RETURN _ |
             T.T_IDENTIFIER _ | T.T_CONTINUE _ 
            ) when not (Hashtbl.mem hrparens_if info)
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push2 (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push2 x res;


        | T.T_RBRACKET _, 
            (T.T_FOR _ | T.T_IF _ | T.T_VAR _ | T.T_IDENTIFIER _)
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push2 (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push2 x res;


        | (T.T_IDENTIFIER _ | T.T_NULL _ | T.T_STRING _ | T.T_REGEX _
            | T.T_FALSE _ | T.T_TRUE _
          ), 
              (T.T_VAR _ | T.T_IDENTIFIER _ | T.T_IF _ | T.T_THIS _ |
                  T.T_RETURN _ | T.T_BREAK _ | T.T_ELSE _
              )
            ->
            let line2 = TH.line_of_tok x in
            let line1 = TH.line_of_tok prev in
            if line2 <> line1
            then begin
              let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
              Common.push2 (T.T_VIRTUAL_SEMICOLON fake) res;
            end;
            Common.push2 x res;

        | _, _ ->        
            Common.push2 x res;
      )
      in
      aux y f ys;
      List.rev !res

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
  let filelines = Common2.cat_array filename in

  let toks_orig = tokens filename in
  let toks = adjust_tokens toks_orig in

  let tr = PI.mk_tokens_state toks in

  let checkpoint = TH.line_of_tok tr.PI.current in

  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in

  let elems = 
    try (
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Left 
        (Common.profile_code "Parser_js.main" (fun () ->
          (Parser_js.main (lexer_function tr) lexbuf_fake)
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

      (xs, toks),
      stat
  | Right (info_of_bads, line_error, cur, exn) ->

      if not !Flag.error_recovery
      then raise (Parse_error (TH.info_of_tok cur));

      (match exn with
      | Lexer_js.Lexical _ 
      | Parsing.Parse_error 
          (*| Semantic_c.Semantic _  *)
        -> ()
      | e -> raise e
      );

      if !Flag.show_parsing_error
      then 
        (match exn with
        (* Lexical is not anymore launched I think *)
        | Lexer_js.Lexical s -> 
            pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
        | Parsing.Parse_error -> 
            pr2 ("parse error \n = " ^ error_msg_tok cur)
              (* | Semantic_java.Semantic (s, i) -> 
                 pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
          *)
        | e -> raise Impossible
        );
      let checkpoint2 = Common.cat filename +> List.length in

      if !Flag.show_parsing_error
      then PI.print_bad line_error (checkpoint, checkpoint2) filelines;

      stat.PI.bad     <- Common.cat filename +> List.length;

      let info_item = List.rev tr.PI.passed in 
      ([Ast.NotParsedCorrectly info_of_bads], info_item), 
      stat

let parse a = 
  Common.profile_code "Parse_js.parse" (fun () -> parse2 a)

let parse_program file = 
  let ((ast, toks), _stat) = parse file in
  ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

(* use program_of_string when you can *)
let tmp_file_from_string s =
  let tmp_file = Common.new_temp_file "test" ".js" in
  Common.write_file ~file:tmp_file s;
  tmp_file

let (program_of_string: string -> Ast_js.program) = fun s -> 
  let tmpfile = tmp_file_from_string s in
  let ((ast, _), _stat) = parse tmpfile in
  Common.erase_this_temp_file tmpfile;
  ast

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

let parse_fuzzy file =
  let toks_orig = tokens file in
  let toks = 
    toks_orig +> Common.exclude (fun x ->
      Token_helpers_js.is_comment x ||
      Token_helpers_js.is_eof x
    )
  in
  let trees = Lib_parser.mk_trees { Lib_parser.
     tokf = TH.info_of_tok;
     kind = (function
      | T.T_LCURLY _ -> Lib_parser.LBrace
      | T.T_RCURLY _ -> Lib_parser.RBrace
      | T.T_LPAREN _ -> Lib_parser.LPar
      | T.T_RPAREN _ -> Lib_parser.RPar
      | _ -> Lib_parser.Other
     );
  } toks 
  in
  trees, toks_orig
