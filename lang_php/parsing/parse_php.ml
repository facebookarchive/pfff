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

(*e: parse_php module aliases *)

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
      
      let ii = Lib_parsing_php.ii_of_toplevel ast in
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

let print_bad line_error (start_line, end_line) filelines  = 
  begin
    pr2 ("badcount: " ^ i_to_s (end_line - start_line));

    for i = start_line to end_line do 
      let line = filelines.(i) in 

      if i =|= line_error 
      then  pr2 ("BAD:!!!!!" ^ " " ^ line) 
      else  pr2 ("bad:" ^ " " ^      line) 
    done
  end
(*e: parse_php error diagnostic *)

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
(*s: type parsing_stat *)
type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
}
(*e: type parsing_stat *)

(*s: parse_php stat function *)
let default_stat file =  { 
    filename = file;
    correct = 0; bad = 0;
(*
    have_timeout = false;
    commentized = 0;
    problematic_lines = [];
*)
  }
(*x: parse_php stat function *)
let print_parsing_stat_list statxs =
  let total = List.length statxs in
  let perfect = 
    statxs 
      +> List.filter (function 
      | {bad = n} when n = 0 -> true 
      | _ -> false)
      +> List.length 
  in

  pr2 "\n\n\n---------------------------------------------------------------";
  pr2 (
  (spf "NB total files = %d; " total) ^
  (spf "perfect = %d; " perfect) ^
  (spf "=========> %d" ((100 * perfect) / total)) ^ "%"
  );

  let good = statxs +> List.fold_left (fun acc {correct = x} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x} -> acc+x) 0  in

  let gf, badf = float_of_int good, float_of_int bad in
  pr2 (
  (spf "nb good = %d,  nb bad = %d " good bad) ^
  (spf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )
(*e: parse_php stat function *)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
(*s: function tokens *)
let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    Lexer_php.reset();
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
        { ii with Ast.pinfo=
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
let tokens a = 
  Common.profile_code "Parse_php.tokens" (fun () -> tokens2 a)
(*e: function tokens *)

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)
(*s: parse tokens_state helper *)
type tokens_state = {
  mutable rest :         Parser_php.token list;
  mutable current :      Parser_php.token;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed :       Parser_php.token list;
  (* if want to do some lalr(k) hacking ... cf yacfe.
   * mutable passed_clean : Parser_php_c.token list;
   * mutable rest_clean :   Parser_php_c.token list;
   *)
}
(*x: parse tokens_state helper *)
let mk_tokens_state toks = 
  { 
    rest       = toks;
    current    = (List.hd toks);
    passed = []; 
    (* passed_clean = [];
     * rest_clean = (toks +> List.filter TH.is_not_comment);
     *)
  }
(*x: parse tokens_state helper *)
(* Hacked lex. This function use refs passed by parse.
 * 'tr' means 'token refs'.
 *)
let rec lexer_function tr = fun lexbuf ->
  match tr.rest with
  | [] -> (pr2 "LEXER: ALREADY AT END"; tr.current)
  | v::xs -> 
      tr.rest <- xs;
      tr.current <- v;
      tr.passed <- v::tr.passed;

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
(* Preprocessor (e.g. XHP) helper functions *)
(*****************************************************************************)


(* update: now that pfff supports XHP construts directly, there is far
 * less need for preprocessor support and merging tokens.
 * 
 * 
 * Pfff allows the user to pass a preprocessor command on the command line,
 * which makes it possible to parse for instance XHP code.
 * 
 * The preprocessor will generate a file in /tmp which means
 * errors or further analysis will report position information
 * on this tmp file. This can be inconvenient. If the
 * preprocessor maintain line positions (which is the mostly case for instance
 * with XHP), we can slightly improve the situation by
 * changing the .file field in parse_info.
 * 
 * Preprocessors such as XHP also remove comments, spaces, and indentations.
 * It can be useful to merge the original comments/spaces/indents in the 
 * original file with the tokens in the expanded file. This makes it easier
 * to support refactoring on XHP code for most of the tokens. Our solution
 * to merge is: 
 *  - transform certain tokens in the original file as 
 *    TCommentPP tokens when they were transformed or passed by
 *    the preprocessor,
 *  - mark the new tokens from the preprocessed code with a ExpandedTok
 *  - adjust the file and position information from similar tokens with
 *    the information from the original file.
 * 
 * Merging is not always possible if the preprocessor introduce really new
 * tokens than the original PHP parser does not understand. In the case
 * of XHP, which mainly use '<', ':'. '>' and strings, this is mostly ok.
 * Nevertheless the original code can also contain 
 * quotes inside HTML snippet which confuse completely the 
 * original lexer which then produce tokens that are too different
 * to the tokens after preprocessing (see tests/xhp/xhp_line_bug4.php). 
 * It is then too hard to synchronize them. What is important 
 * when we have problems synchronizing, is to fall back to a default mode
 * where everything is marked as ExpandedTok and TCommentPP.
 *)


(* a few helpers *)

let group_tokens_by_line toks = 

  let rec aux current_line current_toks  xs =
    match xs with
    | [] -> 
        [current_line,  List.rev current_toks]
    | x::xs ->
        let line = TH.line_of_tok x in
        (match line <=> current_line with
        | Common.Inf -> 
            failwith ("Impossible: wrong order of tokens: " ^ TH.str_of_tok x)
        | Common.Equal ->
            aux current_line (x::current_toks) xs
        | Common.Sup -> 
            let hd = current_line, List.rev current_toks in
            hd::aux line [x] xs
        )
  in
  aux 1 [] toks

let comment_pp_ize toks = 
  toks |> List.map (fun tok -> 
    let info = TH.info_of_tok tok in 
    Parser_php.TCommentPP info
  )

let mark_as_expanded last_orig_parse_info toks =
  let cnt = ref (String.length (last_orig_parse_info.Parse_info.str)) in
  toks |> List.map (fun tok -> 
    let len = String.length (TH.str_of_tok tok) in
    cnt := !cnt + len;
    tok |> TH.visitor_info_of_tok (fun info -> 
      let parse_info_in_pp = Ast.parse_info_of_info info in
      { info with 
        Ast.pinfo = Parse_info.ExpandedTok (
          parse_info_in_pp, 
          last_orig_parse_info, 
          !cnt)
      }
    )
  )


(* Merging tokens on a single line.
 *
 * As a first cut, we just want to know if this line has the same non-comment 
 * tokens in the original and preprocessed code. In that case the line 
 * didn't really needed XHP so we can return the original
 * set of tokens which will have better indentation, comments, etc.
 * Otherwise we return the tokens in the preprocessed code, but with
 * a special mark, ExpandedTok. We also add the original tokens 
 * as TCommentPP so that the unparser can still print back those
 * original tokens and pass the ExpandedTok one.
 * 
 * todo: could try to be more precise and merge more tokens inside
 * a line modified by XHP. Could do a diff at token level 
 * and adjust accordingly.
 * 
 *)
let merge_tokens_line ~orig_toks ~pp_toks = 
  let a = orig_toks in
  let b = pp_toks in

  let al_info_tok tok = 
    TH.visitor_info_of_tok (fun info -> Ast.al_info info) tok
  in
  
  let a' = Common.exclude TH.is_comment a |> List.map al_info_tok in
  let b' = Common.exclude TH.is_comment b |> List.map al_info_tok in

  if a' =*= b'
  then a
  else
    (* todo: could do finer grained things here *)
    let commented_a = comment_pp_ize a in
    if null commented_a
    then failwith "WEIRD: a XHP line has tokens but not the original one";
    
    let last_orig_info = Common.last commented_a |> TH.info_of_tok in
    let last_orig_parse_info = Ast.parse_info_of_info last_orig_info in

    let expanded_b = mark_as_expanded last_orig_parse_info b in

    commented_a ++ expanded_b



let  zip_and_sync ~toks_orig_lines ~toks_pp_lines = 
  (* old: List.map snd toks_pp_lines +> List.flatten *)

  (* This is used below just in one very ugly situation *)
  let last_orig_tok = ref
    (match toks_orig_lines with
    | (xline, xtoks)::xs -> 
        List.hd xtoks
    | _ -> failwith  
        "Impossible: if the file is empty then we should not be called at all"
    )
  in

  let rec aux xs ys = 
    match (xs, ys) with
    | [], [] -> []

    | ((xline, xtoks)::xs), [] -> 
        (* The original can have some comments at the end of the file, which
         * would be removed by XHP
         *
         * TODO: assert only space or eof 
         *)
        xtoks::aux xs []

    | [], (yline, ytoks)::ys -> 
        (* XHP usually cut the space and comments at the end of the file 
         * so the original file should always be longer.
         * 
         * Nevertheless, in certain situations where the tokens 
         * are very different with very different line positions, 
         * sync can get confused. See xhp_line_bug4.php. So the best
         * we can do here is to fall back to our invariant and 
         * mark the tokens as ExpandedTok.
         *)
        let all_remaining_toks = 
          (ytoks::List.map snd ys) |> List.flatten
        in

        let last_orig_parse_info = 
          !last_orig_tok |> TH.info_of_tok |> Ast.parse_info_of_info in
        let toks = mark_as_expanded last_orig_parse_info all_remaining_toks in
        [toks]
        
        
    | (((xline, xtoks)::xs) as a), (((yline, ytoks)::ys) as b) -> 
        last_orig_tok := Common.last xtoks;
        (match xline <=> yline with
        | Inf ->
            (* Sometimes XHP just remove certain tokens, like
             * lines with   attribute x y; in which case we must
             * remove them also from the original file. 
             * 
             * It's also usually because comments are passed by XHP.
             * We could maybe assert to have only space  here or 
             * tokens known to be passed by XHP like attribute
             *)
            let xtoks' = comment_pp_ize xtoks in
            xtoks'::aux xs b
        | Equal ->
            let merged = merge_tokens_line ~orig_toks:xtoks ~pp_toks:ytoks in
            merged::aux xs ys
        | Sup ->
            (* sometimes XHP remove some lines ... so have to adjust things 
             *
             *)
            pr2 (spf "WEIRD, wrong line numbers in preprocessed file %d > %d"
                     xline yline);
                    
            let b' = b |> List.map (fun (yline, ytoks) -> yline+1, ytoks) in
            aux a b'
        )
  in
  aux toks_orig_lines toks_pp_lines +> List.flatten


(* Trying to merge tokens from the original file with the preprocessed file,
 * for instance to put back space, indentation and comments information
 * in the preprocessed file.
 *)
let adapt_tokens_pp2 ~orig_filename toks_pp = 

  (* The old algorithm was just to adjust the .file field so that error 
   * reporting was slightly improved.
   *)
  if not !Flag.obsolete_merge_tokens_xhp then
   toks_pp +> List.rev_map (fun tok ->
    tok +> TH.visitor_info_of_tok (fun ii ->
      let pinfo = Ast.pinfo_of_info ii in
      { ii with Ast.pinfo =
          match pinfo with
          | Parse_info.OriginTok pi ->
              Parse_info.OriginTok { pi with
                Parse_info.file = orig_filename;
              }
          | Parse_info.FakeTokStr _
          | Parse_info.Ab  
            -> pinfo

          | Parse_info.ExpandedTok _ -> 
              raise Impossible
      })
  )  +> List.rev (* ugly, but need tail-call rev_map and so this rev *)
  else 
    (* algo: 
     *  - split by line the tokens 
     *  - for each line try to synchronize, 
     *    by marking as TCommentPP the relevant tokens from toks_orig
     *    and adding one from toks as ExpandedTok
     *)
    let toks_orig = tokens orig_filename in

    let toks_pp_lines   = group_tokens_by_line toks_pp in
    let toks_orig_lines = group_tokens_by_line toks_orig in
    zip_and_sync ~toks_orig_lines ~toks_pp_lines

let adapt_tokens_pp ~orig_filename toks_pp =
  Common.profile_code "Parse_php.merge tokens" (fun () -> 
    adapt_tokens_pp2 ~orig_filename toks_pp)

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

  let stat = default_stat filename in
  let filelines = Common.cat_array filename in

  let toks = tokens filename in
  let toks = 
    if filename = orig_filename
    then toks
    else adapt_tokens_pp ~orig_filename toks
  in

  let tr = mk_tokens_state toks in

  let checkpoint = TH.line_of_tok tr.current in

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

      let line_error = TH.line_of_tok tr.current in

      let _passed_before_error = tr.passed in
      let current = tr.current in

      (* no error recovery, the whole file is discarded *)
      tr.passed <- List.rev toks;

      let info_of_bads = Common.map_eff_rev TH.info_of_tok tr.passed in 

      Right (info_of_bads, line_error, current, e)
  in

  match elems with
  | Left xs ->
      stat.correct <- (Common.cat filename +> List.length);

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
      then print_bad line_error (checkpoint, checkpoint2) filelines;

      stat.bad     <- Common.cat filename +> List.length;

      let info_item = mk_info_item filename (List.rev tr.passed) in 
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

(* This function is useful not only to test but also in our own code
 * as a shortcut to build complex expressions
 *)
let (expr_of_string: string -> Ast_php.expr) = fun s ->
  let tmpfile = Common.new_temp_file "pff_expr_of_s" "php" in
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
  let tmpfile = Common.new_temp_file "pff_expr_of_s" "php" in
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
