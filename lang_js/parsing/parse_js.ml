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

module Ast = Ast_js
module Flag = Flag_parsing_js
module TH   = Token_helpers_js

module T = Parser_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Lots of copy paste with my other parsers (e.g. PHP, C, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program2 = toplevel2 list
  and toplevel2 = 
    Ast_js.toplevel (* NotParsedCorrectly if parse error *) * info_item
     (* the token list contains also the comment-tokens *)
     and info_item = (string * Parser_js.token list)

let program_of_program2 xs = 
  xs +> List.map fst

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_err, pr2_once = Common.mk_pr2_wrappers Flag.verbose_parsing 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let token_to_strpos tok = 
  (TH.str_of_tok tok, TH.pos_of_tok tok)

let mk_info_item2 filename toks = 
  let buf = Buffer.create 100 in
  let s = 
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks +> List.iter (fun tok -> 
        match TH.pinfo_of_tok tok with
        | Ast.OriginTok _ 
        | Ast.ExpandedTok _ ->
            Buffer.add_string buf (TH.str_of_tok tok)

        (* the virtual semicolon *)
        | Ast.FakeTokStr _ -> 
            ()

        | Ast.Ab _  -> raise Impossible
      );
      Buffer.contents buf
    end
  in
  (s, toks) 

let mk_info_item a b = 
  Common.profile_code "Parsing.mk_info_item" 
    (fun () -> mk_info_item2 a b)

(* on very huge file, this function was previously segmentation fault
 * in native mode because span was not tail call
 *)
let rec distribute_info_items_toplevel2 xs toks filename = 
  match xs with
  | [] -> raise Impossible
  | [Ast_js.FinalDef e] -> 
      (* assert (null toks) ??? no cos can have whitespace tokens *) 
      let info_item = mk_info_item filename toks in
      [Ast_js.FinalDef e, info_item]
  | ast::xs ->
      (match ast with
      | Ast_js.St (Ast_js.Nop None) -> 
          distribute_info_items_toplevel2 xs toks filename
      | _ ->

          let ii = Lib_parsing_js.ii_of_toplevel ast in
          let (min, max) = Lib_parsing_js.min_max_ii_by_pos ii in
          
          let toks_before_max, toks_after = 
            Common.profile_code "spanning tokens" (fun () ->
              toks +> Common.span_tail_call (fun tok ->
                match Ast_js.compare_pos (TH.info_of_tok tok) max with
                | -1 | 0 -> true
                | 1 -> false
                | _ -> raise Impossible
              ))
          in
          let info_item = mk_info_item filename toks_before_max in
          (ast, info_item)::distribute_info_items_toplevel2 xs toks_after filename
      )

let distribute_info_items_toplevel a b c = 
  Common.profile_code "distribute_info_items" (fun () -> 
    distribute_info_items_toplevel2 a b c
  )

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)

let error_msg_tok tok = 
  let file = TH.file_of_tok tok in
  if !Flag.verbose_parsing
  then Common.error_message file (token_to_strpos tok) 
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

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)

type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
}

let default_stat file =  { 
    filename = file;
    correct = 0; bad = 0;
}

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

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Common.full_charpos_to_pos_large file in

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
        { ii with Ast.pinfo=
          (* could assert pinfo.filename = file ? *)
               match Ast.pinfo_of_info ii with
               | Ast.OriginTok pi ->
                          Ast.OriginTok 
                            (Common.complete_parse_info_large file table pi)
               | Ast.FakeTokStr _
               | Ast.Ab  
               | Ast.ExpandedTok _
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
                   (Common.error_message file (lexbuf_to_strpos lexbuf)))
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

  toks +> Common.iter_with_previous_opt (fun prev x -> 
    (match x with
    | T.T_LPAREN _ -> 
        Common.push2 prev stack;
    | T.T_RPAREN info ->
        if !stack <> [] then begin
        let top = Common.pop2 stack in
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

type tokens_state = {
  mutable rest :         Parser_js.token list;
  mutable current :      Parser_js.token;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed :       Parser_js.token list;
  (* if want to do some lalr(k) hacking ... cf yacfe.
   * mutable passed_clean : Parser_js_c.token list;
   * mutable rest_clean :   Parser_js_c.token list;
   *)
}

let mk_tokens_state toks = 
  { 
    rest       = toks;
    current    = (List.hd toks);
    passed = []; 
    (* passed_clean = [];
     * rest_clean = (toks +> List.filter TH.is_not_comment);
     *)
  }

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

      if TH.is_comment v (* || other condition to pass tokens ? *)
      then lexer_function (*~pass*) tr lexbuf
      else v

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =

  let stat = default_stat filename in
  let filelines = Common.cat_array filename in

  let toks_orig = tokens filename in
  let toks = adjust_tokens toks_orig in

  let tr = mk_tokens_state toks in

  let checkpoint = TH.line_of_tok tr.current in

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

      distribute_info_items_toplevel xs toks_orig filename, 
      stat
  | Right (info_of_bads, line_error, cur, exn) ->

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
      then print_bad line_error (checkpoint, checkpoint2) filelines;

      stat.bad     <- Common.cat filename +> List.length;

      let info_item = mk_info_item filename (List.rev tr.passed) in 
      [Ast.NotParsedCorrectly info_of_bads, info_item], 
      stat

let parse a = 
  Common.profile_code "Parse_js.parse" (fun () -> parse2 a)


let parse_program file = 
  let (ast2, _stat) = parse file in
  program_of_program2 ast2

(*****************************************************************************)
