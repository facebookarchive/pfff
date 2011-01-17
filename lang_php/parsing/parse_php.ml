(*s: parse_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)

open Common 

(*s: parse_php module aliases *)
module Ast  = Ast_php
module Flag = Flag_parsing_php
module TH   = Token_helpers_php

open Ast_php

(*e: parse_php module aliases *)

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*s: type program2 *)
type program2 = toplevel2 list
  and toplevel2 = 
    Ast_php.toplevel (* NotParsedCorrectly if parse error *) * info_item
     (* the token list contains also the comment-tokens *)
     and info_item = (string * Parser_php.token list)
(*e: type program2 *)

(*s: function program_of_program2 *)
let program_of_program2 xs = 
  xs +> List.map fst
(*e: function program_of_program2 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_err, pr2_once = Common.mk_pr2_wrappers Flag.verbose_parsing 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: parse_php helpers *)
let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let token_to_strpos tok = 
  (TH.str_of_tok tok, TH.pos_of_tok tok)
(*x: parse_php helpers *)
let mk_info_item2 filename toks = 
  let buf = Buffer.create 100 in
  let s = 
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks +> List.iter (fun tok -> 
        match TH.pinfo_of_tok tok with
        | Parse_info.OriginTok _ 
        | Parse_info.ExpandedTok _ ->
            Buffer.add_string buf (TH.str_of_tok tok)

        | Parse_info.Ab _ | Parse_info.FakeTokStr _ -> raise Impossible
      );
      Buffer.contents buf
    end
  in
  (s, toks) 

let mk_info_item a b = 
  Common.profile_code "Parsing.mk_info_item" 
    (fun () -> mk_info_item2 a b)
(*x: parse_php helpers *)
(* on very huge file, this function was previously segmentation fault
 * in native mode because span was not tail call
 *)
let rec distribute_info_items_toplevel2 xs toks filename = 
  match xs with
  | [] -> raise Impossible
  | [Ast_php.FinalDef e] -> 
      (* assert (null toks) ??? no cos can have whitespace tokens *) 
      let info_item = mk_info_item filename toks in
      [Ast_php.FinalDef e, info_item]
  | ast::xs ->
      
      let ii = Lib_parsing_php.ii_of_any (Ast.Toplevel ast) in
      let (min, max) = Lib_parsing_php.min_max_ii_by_pos ii in

      let toks_before_max, toks_after = 
        Common.profile_code "spanning tokens" (fun () ->
        toks +> Common.span_tail_call (fun tok ->
          match Ast_php.compare_pos (TH.info_of_tok tok) max with
          | -1 | 0 -> true
          | 1 -> false
          | _ -> raise Impossible
        ))
      in
      let info_item = mk_info_item filename toks_before_max in
      (ast, info_item)::distribute_info_items_toplevel2 xs toks_after filename

let distribute_info_items_toplevel a b c = 
  Common.profile_code "distribute_info_items" (fun () -> 
    distribute_info_items_toplevel2 a b c
  )
(*e: parse_php helpers *)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
(*s: parse_php error diagnostic *)
let error_msg_tok tok = 
  let file = TH.file_of_tok tok in
  if !Flag.verbose_parsing
  then Parse_info.error_message file (token_to_strpos tok) 
  else ("error in " ^ file  ^ "set verbose_parsing for more info")

(*e: parse_php error diagnostic *)

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
(*s: type parsing_stat *)
(*e: type parsing_stat *)

(*s: parse_php stat function *)
(*x: parse_php stat function *)
(*e: parse_php stat function *)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
(*s: function tokens *)
let tokens2 ?(init_state=Lexer_php.INITIAL) file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    Lexer_php.reset();
    Lexer_php._mode_stack := [init_state];

    try 
      (*s: function phptoken *)
      let phptoken lexbuf = 
        (*s: yyless trick in phptoken *)
          (* for yyless emulation *)
          match !Lexer_php._pending_tokens with
          | x::xs -> 
              Lexer_php._pending_tokens := xs; 
              x
          | [] ->
        (*e: yyless trick in phptoken *)
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
      (*e: function phptoken *)

      let rec tokens_aux acc = 
        let tok = phptoken lexbuf in

        if !Flag.debug_lexer then Common.pr2_gen tok;
        if not (TH.is_comment tok)
        then Lexer_php._last_non_whitespace_like_token := Some tok;

        (*s: fill in the line and col information for tok *)
        let tok = tok +> TH.visitor_info_of_tok (fun ii ->
        { ii with Parse_info.token=
          (* could assert pinfo.filename = file ? *)
               match Ast.pinfo_of_info ii with
               | Parse_info.OriginTok pi ->
                          Parse_info.OriginTok 
                            (Parse_info.complete_parse_info_large file table pi)
               | Parse_info.FakeTokStr _
               | Parse_info.Ab  
               | Parse_info.ExpandedTok _
                        -> raise Impossible
                  })
        in
        (*e: fill in the line and col information for tok *)

        if TH.is_eof tok
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
    in
    tokens_aux []
  with
  | Lexer_php.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                   (Parse_info.error_message file (lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )
(*x: function tokens *)
let tokens ?init_state a = 
  Common.profile_code "Parse_php.tokens" (fun () -> tokens2 ?init_state a)
(*e: function tokens *)

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)
(*s: parse tokens_state helper *)
(*x: parse tokens_state helper *)
(*x: parse tokens_state helper *)
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

(*e: parse tokens_state helper *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(*s: Parse_php.parse *)

let parse2 ?(pp=(!Flag.pp_default)) filename =

  let orig_filename = filename in
  let filename =
    match pp with
    | None -> orig_filename
    | Some cmd ->
        (* note that now that pfff support XHP constructs directly, 
         * this code is not that needed.
         *)
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

  let stat = Parse_info.default_stat filename in
  let filelines = Common.cat_array filename in

  let toks = tokens filename in
  let toks = 
    if filename = orig_filename
    then toks
    else Pp_php.adapt_tokens_pp ~tokenizer:tokens ~orig_filename toks
  in

  let tr = Parse_info.mk_tokens_state toks in

  let checkpoint = TH.line_of_tok tr.PI.current in

  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in
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

      let info_of_bads = Common.map_eff_rev TH.info_of_tok tr.PI.passed in 

      Right (info_of_bads, line_error, current, e)
  in

  match elems with
  | Left xs ->
      stat.PI.correct <- (Common.cat filename +> List.length);

      distribute_info_items_toplevel xs toks filename, 
      stat
  | Right (info_of_bads, line_error, cur, exn) ->

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
        | e -> raise Impossible
        );
      let checkpoint2 = Common.cat filename +> List.length in

      if !Flag.show_parsing_error
      then Parse_info.print_bad line_error (checkpoint, checkpoint2) filelines;

      stat.PI.bad     <- Common.cat filename +> List.length;

      let info_item = mk_info_item filename (List.rev tr.PI.passed) in 
      [Ast.NotParsedCorrectly info_of_bads, info_item], 
      stat
(*x: Parse_php.parse *)

let _hmemo_parse_php = Hashtbl.create 101

let parse_memo ?pp file = 
  if not !Flag.caching_parsing
  then parse2 ?pp file
  else
    Common.memoized _hmemo_parse_php file (fun () -> 
      parse2 ?pp file
    )

let parse ?pp a = 
  Common.profile_code "Parse_php.parse" (fun () -> parse_memo ?pp a)
(*e: Parse_php.parse *)

let parse_program ?pp file = 
  let (ast2, _stat) = parse ?pp file in
  program_of_program2 ast2

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let parse_any filename =
  let toks = tokens ~init_state:Lexer_php.ST_IN_SCRIPTING filename in

  let tr = PI.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in
  
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
    
let any_of_string s =
  let tmpfile = Common.new_temp_file "pfff_any_of_s" "php" in
  Common.write_file tmpfile s;
  let res = parse_any tmpfile in
  Common.erase_this_temp_file tmpfile;
  res

(* 
 * todo: obsolete now with parse_any ? just redirect to parse_any ?
 * 
 * This function is useful not only to test but also in our own code
 * as a shortcut to build complex expressions
 *)
let (expr_of_string: string -> Ast_php.expr) = fun s ->
  let tmpfile = Common.new_temp_file "pfff_expr_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ ";\n");

  let (ast2, _stat) = parse tmpfile in
  let ast = program_of_program2 ast2 in

  let res = 
    (match ast with
    | [Ast.StmtList [Ast.ExprStmt (e, _tok)];Ast.FinalDef _] -> e
  | _ -> failwith "only expr pattern are supported for now"
  )
  in
  Common.erase_this_temp_file tmpfile;
  res

(* this function is useful mostly for our unit tests *)
let (program_of_string: string -> Ast_php.program) = fun s -> 
  let tmpfile = Common.new_temp_file "pfff_expr_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ "\n");
  let (ast2, _stat) = parse tmpfile in
  let ast = program_of_program2 ast2 in
  Common.erase_this_temp_file tmpfile;
  ast
  

(* 
 * The regular lexer 'tokens' at the beginning of this file is quite
 * complicated because it has to maintain a state (for the HereDoc, 
 * interpolated string, HTML switching mode, etc) and it also takes 
 * a file not a string because it annotates tokens with file position.
 * Sometimes we need only a simple and faster lexer and one that can 
 * take a string hence this function.
 *)
let rec basic_lexer_skip_comments lexbuf = 
  let tok = Lexer_php.st_in_scripting lexbuf in
  if TH.is_comment tok 
  then basic_lexer_skip_comments lexbuf
  else tok

(* A fast-path parser of xdebug expressions in xdebug dumpfiles. 
 * See xdebug.ml *)
let (xdebug_expr_of_string: string -> Ast_php.expr) = fun s ->
  let lexbuf = Lexing.from_string s in
  let expr = Parser_php.expr basic_lexer_skip_comments lexbuf in
  expr


let (class_def_of_string: string -> Ast_php.class_def) = fun s ->
  let lexbuf = Lexing.from_string s in
  let x = 
    Parser_php.class_declaration_statement basic_lexer_skip_comments lexbuf in
  match x with
  | Left class_def -> class_def
  | Right interface_def -> 
      failwith "was expecting a class def, not an interface"

(*e: parse_php.ml *)
