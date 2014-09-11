(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2011 Facebook
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

module Ast  = Ast_php
module Flag = Flag_parsing_php
module TH   = Token_helpers_php
module T = Parser_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* A PHP parser.
 *
 * related work:
 *  - miamide, also in ocaml, but didn't support all of PHP
 *  - https://github.com/sfindeisen/phphard, also written in ocaml, but
 *    seems pretty rudimentary
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type program_with_comments = Ast_php.program * Parser_php.token list

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = 
  PI.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
let tokens_from_changen ?(init_state=Lexer_php.INITIAL) changen =
  let table     = PI.full_charpos_to_pos_large_from_changen changen in

  let (chan, _, file) = changen () in

  Common.finalize (fun () ->
    let lexbuf = Lexing.from_channel chan in

    Lexer_php.reset();
    Lexer_php._mode_stack := [init_state];

    try 
      let phptoken lexbuf = 
          (* for yyless emulation *)
          match !Lexer_php._pending_tokens with
          | x::xs -> 
              Lexer_php._pending_tokens := xs; 
              x
          | [] ->
            (match Lexer_php.current_mode () with
            | Lexer_php.INITIAL -> 
                Lexer_php.initial lexbuf
            | Lexer_php.ST_IN_SCRIPTING -> 
                Lexer_php.st_in_scripting lexbuf
            | Lexer_php.ST_IN_SCRIPTING2 -> 
                Lexer_php.st_in_scripting lexbuf
            | Lexer_php.ST_DOUBLE_QUOTES -> 
                Lexer_php.st_double_quotes lexbuf
            | Lexer_php.ST_BACKQUOTE -> 
                Lexer_php.st_backquote lexbuf
            | Lexer_php.ST_LOOKING_FOR_PROPERTY -> 
                Lexer_php.st_looking_for_property lexbuf
            | Lexer_php.ST_LOOKING_FOR_VARNAME -> 
                Lexer_php.st_looking_for_varname lexbuf
            | Lexer_php.ST_VAR_OFFSET -> 
                Lexer_php.st_var_offset lexbuf
            | Lexer_php.ST_START_HEREDOC s ->
                Lexer_php.st_start_heredoc s lexbuf
            | Lexer_php.ST_START_NOWDOC s ->
                Lexer_php.st_start_nowdoc s lexbuf

            (* xhp: *)
            | Lexer_php.ST_IN_XHP_TAG current_tag ->
                if not !Flag.xhp_builtin
                then raise Impossible;

                Lexer_php.st_in_xhp_tag current_tag lexbuf
            | Lexer_php.ST_IN_XHP_TEXT current_tag ->
                if not !Flag.xhp_builtin
                then raise Impossible;

                Lexer_php.st_in_xhp_text current_tag lexbuf
            )
      in

      let rec tokens_aux acc = 
        let tok = phptoken lexbuf in

        if !Flag.debug_lexer then Common.pr2_gen tok;
        if not (TH.is_comment tok)
        then Lexer_php._last_non_whitespace_like_token := Some tok;

        let tok = tok +> TH.visitor_info_of_tok (fun ii ->
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
               match ii.PI.token with
               | PI.OriginTok pi ->
                          PI.OriginTok 
                            (PI.complete_token_location_large file table pi)
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
  | Lexer_php.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                   (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )
 (fun () -> close_in chan)

let tokens2 ?init_state =
  PI.file_wrap_changen (tokens_from_changen ?init_state)

let tokens ?init_state a = 
  Common.profile_code "Parse_php.tokens" (fun () -> tokens2 ?init_state a)

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

      if TH.is_comment v ||
        (* TODO a little bit specific to FB ? *)
        (match v with
        | Parser_php.T_OPEN_TAG _ -> true
        | Parser_php.T_CLOSE_TAG _ -> true
        | _ -> false
        )
      then lexer_function (*~pass*) tr lexbuf
      else v

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* could move that in h_program-lang/, but maybe clearer to put it closer
 * to the parsing function.
 *)
exception Parse_error of PI.info

let parse2 ?(pp=(!Flag.pp_default)) filename =

  let orig_filename = filename in
  let filename =
    (* note that now that pfff support XHP constructs directly, 
     * this code is not that needed.
     *)
    match pp with
    | None -> orig_filename
    | Some cmd ->
        Common.profile_code "Parse_php.pp_maybe" (fun () ->

          let pp_flag = if !Flag.verbose_pp then "-v" else "" in

          (* The following requires the preprocessor command to
           * support the -q command line flag.
           * 
           * Maybe a little bit specific to XHP and xhpize ... But
           * because I use as a convention that 0 means no_need_pp, if
           * the preprocessor does not support -q, it should return an
           * error code, in which case we will fall back to the regular
           * case. *)
          let cmd_need_pp = 
            spf "%s -q %s %s" cmd pp_flag filename in
          if !Flag.verbose_pp then pr2 (spf "executing %s" cmd_need_pp);
          let ret = Sys.command cmd_need_pp in
          if ret = 0 
          then orig_filename
          else begin
            Common.profile_code "Parse_php.pp" (fun () ->
            let tmpfile = Common.new_temp_file "pp" ".pphp" in
            let fullcmd = 
              spf "%s %s %s > %s" cmd pp_flag filename tmpfile in
            if !Flag.verbose_pp then pr2 (spf "executing %s" fullcmd);
            let ret = Sys.command fullcmd in
            if ret <> 0
            then failwith "The preprocessor command returned an error code";
            tmpfile
            )
          end
        )
  in

  let stat = PI.default_stat filename in
  let filelines = Common2.cat_array filename in

  let toks = tokens filename in
  (* note that now that pfff support XHP constructs directly, 
   * this code is not that needed.
   *)
  let toks = 
    if filename = orig_filename
    then toks
    else Pp_php.adapt_tokens_pp ~tokenizer:tokens ~orig_filename toks
  in
  let toks = Parsing_hacks_php.fix_tokens toks in

  let tr = PI.mk_tokens_state toks in

  let checkpoint = TH.line_of_tok tr.PI.current in

  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in
  let elems = 
    try (
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Left 
        (Common.profile_code "Parser_php.main" (fun () ->
          (Parser_php.main (lexer_function tr) lexbuf_fake)
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
      | Lexer_php.Lexical _ 
      | Parsing.Parse_error 
          (*| Semantic_c.Semantic _  *)
        -> ()
      | e -> raise e
      );

      if !Flag.show_parsing_error
      then 
        (match exn with
        (* Lexical is not anymore launched I think *)
        | Lexer_php.Lexical s -> 
            pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
        | Parsing.Parse_error -> 
            pr2 ("parse error \n = " ^ error_msg_tok cur)
              (* | Semantic_java.Semantic (s, i) -> 
                 pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
          *)
        | _e -> raise Impossible
        );
      let checkpoint2 = Common.cat filename +> List.length in


      if !Flag.show_parsing_error_full
      then PI.print_bad line_error (checkpoint, checkpoint2) filelines;

      stat.PI.bad     <- Common.cat filename +> List.length;

      let info_item = (List.rev tr.PI.passed) in 
      ([Ast.NotParsedCorrectly info_of_bads], info_item), 
      stat

let _hmemo_parse_php = Hashtbl.create 101

let parse_memo ?pp file = 
  if not !Flag.caching_parsing
  then parse2 ?pp file
  else
    Common.memoized _hmemo_parse_php file (fun () -> 
      Common.profile_code "Parse_php.parse_no_memo" (fun () ->
        parse2 ?pp file
      )
    )

let parse ?pp a = 
  Common.profile_code "Parse_php.parse" (fun () -> parse_memo ?pp a)

let parse_program ?pp file = 
  let ((ast, _toks), _stat) = parse ?pp file in
  ast

let ast_and_tokens file =
  let ((ast, toks), _stat) = parse file in
  (ast, toks)

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let parse_any_from_changen (changen : PI.changen) =
  let toks = tokens_from_changen ~init_state:Lexer_php.ST_IN_SCRIPTING changen  in

  let tr = PI.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  try 
    Parser_php.sgrep_spatch_pattern (lexer_function tr) lexbuf_fake
  with exn ->
    let cur = tr.PI.current in
    if !Flag.show_parsing_error
    then 
    (match exn with
     (* Lexical is not anymore launched I think *)
     | Lexer_php.Lexical s -> 
         pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
     | Parsing.Parse_error -> 
         pr2 ("parse error \n = " ^ error_msg_tok cur)
    (* | Semantic_java.Semantic (s, i) -> 
         pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.current)
    *)
     | _ -> raise exn
    );
    raise exn

let parse_any = PI.file_wrap_changen parse_any_from_changen

(* any_of_string() allows small chunks of PHP to be parsed without
 * having to use the filesystem by leveraging the changen mechanism.
 * In order to supply a string as a channel we must create a socket
 * pair and write our string to it.  This is not ideal and may fail if
 * we try to parse too many short strings without closing the channel,
 * or if the string is so large that the OS blocks our socket. 
 *)
let any_of_string s =
  let len = String.length s in
  let changen = (fun () ->
    let (socket_a, socket_b) = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
    let fake_filename = "" in
    let (data_in, data_out) =
      Unix.(in_channel_of_descr socket_a, out_channel_of_descr socket_b) in
    output_string data_out s;
    flush data_out;
    close_out data_out;
    (data_in, len, fake_filename)) in
  (* disable showing parsing errors as there is no filename and
   * error_msg_tok() would throw a Sys_error exception
   *)
  Common.save_excursion Flag.show_parsing_error false (fun () ->
    parse_any_from_changen changen
  )

(* 
 * todo: obsolete now with parse_any ? just redirect to parse_any ?
 * 
 * This function is useful not only to test but also in our own code
 * as a shortcut to build complex expressions
 *)
let (expr_of_string: string -> Ast_php.expr) = fun s ->
  let tmpfile = Common.new_temp_file "pfff_expr_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ ";\n");

  let ast = parse_program tmpfile in

  let res = 
    (match ast with
    | [Ast.StmtList [Ast.ExprStmt (e, _tok)];Ast.FinalDef _] -> e
  | _ -> failwith "only expr pattern are supported for now"
  )
  in
  Common.erase_this_temp_file tmpfile;
  res

(* It is clearer for our testing code to programmatically build source files
 * so that all the information about a test is in the same
 * file. You don't have to open extra files to understand the test
 * data. This function is useful mostly for our unit tests 
*)
let (program_of_string: string -> Ast_php.program) = fun s -> 
  let tmpfile = Common.new_temp_file "pfff_expr_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ "\n");
  let ast = parse_program tmpfile in
  Common.erase_this_temp_file tmpfile;
  ast

(* use program_of_string when you can *)
let tmp_php_file_from_string ?(header="<?php\n") s =
  let tmp_file = Common.new_temp_file "test" ".php" in
  Common.write_file ~file:tmp_file (header ^ s);
  tmp_file


(* this function is useful mostly for our unit tests *)
let (tokens_of_string: string -> Parser_php.token list) = fun s -> 
  let tmpfile = Common.new_temp_file "pfff_tokens_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ "\n");
  let toks = tokens tmpfile in
  Common.erase_this_temp_file tmpfile;
  toks
  

(* A fast-path parser of xdebug expressions in xdebug dumpfiles. 
 * See xdebug.ml *)
let (xdebug_expr_of_string: string -> Ast_php.expr) = fun _s ->
(*
  let lexbuf = Lexing.from_string s in
  let expr = Parser_php.expr basic_lexer_skip_comments lexbuf in
  expr
*)
  raise Todo

(* The default PHP parser function stores position information for all tokens,
 * build some Parse_php.info_items for each toplevel entities, and
 * do other things which are most of the time useful for some analysis
 * but starts to really slow down parsing for huge (generated) PHP files.
 * Enters parse_fast() that disables most of those things.
 * Note that it may not parse correctly all PHP code, so use with
 * caution.
 *)
let parse_fast file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  Lexer_php.reset();
  Lexer_php._mode_stack := [Lexer_php.INITIAL];

  let rec php_next_token lexbuf = 
    let tok =
    (* for yyless emulation *)
    match !Lexer_php._pending_tokens with
    | x::xs -> 
      Lexer_php._pending_tokens := xs; 
      x
    | [] ->
      (match Lexer_php.current_mode () with
      | Lexer_php.INITIAL -> 
        Lexer_php.initial lexbuf
      | Lexer_php.ST_IN_SCRIPTING -> 
        Lexer_php.st_in_scripting lexbuf
      | Lexer_php.ST_IN_SCRIPTING2 -> 
        Lexer_php.st_in_scripting lexbuf
      | Lexer_php.ST_DOUBLE_QUOTES -> 
        Lexer_php.st_double_quotes lexbuf
      | Lexer_php.ST_BACKQUOTE -> 
        Lexer_php.st_backquote lexbuf
      | Lexer_php.ST_LOOKING_FOR_PROPERTY -> 
        Lexer_php.st_looking_for_property lexbuf
      | Lexer_php.ST_LOOKING_FOR_VARNAME -> 
        Lexer_php.st_looking_for_varname lexbuf
      | Lexer_php.ST_VAR_OFFSET -> 
        Lexer_php.st_var_offset lexbuf
      | Lexer_php.ST_START_HEREDOC s ->
        Lexer_php.st_start_heredoc s lexbuf
      | Lexer_php.ST_START_NOWDOC s ->
        Lexer_php.st_start_nowdoc s lexbuf
      | Lexer_php.ST_IN_XHP_TAG current_tag ->
        Lexer_php.st_in_xhp_tag current_tag lexbuf
      | Lexer_php.ST_IN_XHP_TEXT current_tag ->
        Lexer_php.st_in_xhp_text current_tag lexbuf
      )
    in
    match tok with
    | Parser_php.T_COMMENT _ | Parser_php.T_DOC_COMMENT _
    | Parser_php.TSpaces _ | Parser_php.TNewline _
    | Parser_php.TCommentPP _
    | Parser_php.T_OPEN_TAG _
    | Parser_php.T_CLOSE_TAG _ ->
       php_next_token lexbuf
    | _ -> tok
  in
  try 
    let res = Parser_php.main php_next_token lexbuf in
    close_in chan;
    res
  with Parsing.Parse_error ->
    pr2 (spf "parsing error in php fast parser: %s" 
           (Lexing.lexeme lexbuf));
    raise Parsing.Parse_error

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
