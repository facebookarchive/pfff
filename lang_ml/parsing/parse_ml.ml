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

module Ast = Ast_ml
module Flag = Flag_parsing_ml
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

type program2 = toplevel2 list
  (* the token list contains also the comment-tokens *)
  and toplevel2 = 
    Ast.toplevel (* NotParsedCorrectly if parse error *) * Parser_ml.token list

let program_of_program2 xs = 
  xs +> List.map fst

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_err, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing 

(*****************************************************************************)
(* Tokens/Ast association  *)
(*****************************************************************************)

(* on very huge file, this function was previously segmentation fault
 * in native mode because span was not tail call
 *)
let rec distribute_info_items_toplevel2 xs toks filename = 
  match xs with
  | [] -> raise Impossible
  | [Ast_ml.FinalDef e] -> 
      (* assert (null toks) ??? no cos can have whitespace tokens *) 
      let info_item = toks in
      [Ast_ml.FinalDef e, info_item]
  | ast::xs ->

      let ii = Lib_parsing_ml.ii_of_any (Ast_ml.Toplevel ast) in
      let (min, max) = Lib_parsing_ml.min_max_ii_by_pos ii in
          
      let toks_before_max, toks_after = 
        Common.profile_code "spanning tokens" (fun () ->
          toks +> Common2.span_tail_call (fun tok ->
            match Parse_info.compare_pos (TH.info_of_tok tok) max with
            | -1 | 0 -> true
            | 1 -> false
            | _ -> raise Impossible
          ))
      in
      let info_item = toks_before_max in
      (ast, info_item)::distribute_info_items_toplevel2 xs toks_after filename


let distribute_info_items_toplevel a b c = 
  Common.profile_code "distribute_info_items" (fun () -> 
    distribute_info_items_toplevel2 a b c
  )

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)

let token_to_strpos tok = 
  (TH.str_of_tok tok, TH.pos_of_tok tok)

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
      let mltoken lexbuf = 
        Lexer_ml.token lexbuf
      in
      
      let rec tokens_aux acc = 
        let tok = mltoken lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

        let tok = tok +> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
           match PI.pinfo_of_info ii with
           | PI.OriginTok pi ->
               PI.OriginTok 
                 (PI.complete_parse_info_large file table pi)
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
(* Fuzzy parsing *)
(*****************************************************************************)

let is_lbrace = function
  | T.TOBrace _ -> true  | _ -> false
let is_rbrace = function
  | T.TCBrace _ -> true  | _ -> false

let is_lparen = function
  | T.TOParen _ -> true  | _ -> false
let is_rparen = function
  | T.TCParen _ -> true  | _ -> false


let tokf tok =
  TH.info_of_tok tok

(* 
 * less: check that it's consistent with the indentation? 
 * less: more fault tolerance? if col == 0 and { then reset?
 * 
 * Assumes work on a list of tokens without comments.
 * 
 * todo: make this mode independent of ocaml so that we can reuse
 * this code for other languages. I should also factorize with
 * Parse_cpp.parse_fuzzy.
 *)
let mk_trees xs =

  let rec consume x xs =
    match x with
    | tok when is_lbrace tok -> 
        let body, closing, rest = look_close_brace x [] xs in
        Ast_fuzzy.Braces (tokf x, body, tokf closing), rest
    | tok when is_lparen tok ->
        let body, closing, rest = look_close_paren x [] xs in
        let body' = split_comma body in
        Ast_fuzzy.Parens (tokf x, body', tokf closing), rest
    | tok -> 
      Ast_fuzzy.Tok (TH.str_of_tok tok, tokf x), xs
(*
    (match Ast.str_of_info (tokext tok) with
    | "..." -> Ast_fuzzy.Dots (tokext tok)
    | s when Ast_fuzzy.is_metavar s -> Ast_fuzzy.Metavar (s, tokext tok)
    | s -> Ast_fuzzy.Tok (s, tokext tok)
*)
  
  and aux xs =
  match xs with
  | [] -> []
  | x::xs ->
      let x', xs' = consume x xs in
      x'::aux xs'

  and look_close_brace tok_start accbody xs =
    match xs with
    | [] -> 
        failwith (spf "PB look_close_brace (started at %d)" 
                    (TH.line_of_tok tok_start))
    | x::xs -> 
        (match x with
        | tok when is_rbrace tok-> 
          List.rev accbody, x, xs

        | _ -> let (x', xs') = consume x xs in
               look_close_brace tok_start (x'::accbody) xs'
        )

  and look_close_paren tok_start accbody xs =
    match xs with
    | [] -> 
        failwith (spf "PB look_close_paren (started at %d)" 
                     (TH.line_of_tok tok_start))
    | x::xs -> 
        (match x with
        | tok when is_rparen tok -> 
            List.rev accbody, x, xs
        | _ -> 
            let (x', xs') = consume x xs in
            look_close_paren tok_start (x'::accbody) xs'
        )

  and split_comma xs =
     let rec aux acc xs =
       match xs with
       | [] ->
         if null acc
         then []
         else [Left (acc +> List.rev)]
       | x::xs ->
         (match x with
         | Ast_fuzzy.Tok (",", info) ->
           let before = acc +> List.rev in
           if null before
           then aux [] xs
           else (Left before)::(Right (info))::aux [] xs
         | _ ->
           aux (x::acc) xs
         )
     in
     aux [] xs
  in
  aux xs

(* This is similar to what I did for OPA. This is also similar
 * to what I do for parsing hacks forC++, but this fuzzy AST can be useful
 * on its own, e.g. for a not too bad sgrep/spatch.
 *)
let parse_fuzzy file =
  let toks_orig = tokens file in
  let toks = 
    toks_orig +> Common.exclude (fun x ->
      Token_helpers_ml.is_comment x ||
      Token_helpers_ml.is_eof x
    )
  in
  let trees = mk_trees toks in
  trees, toks_orig

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
  let filelines = Common2.cat_array filename in

  let toks = tokens filename in

  let tr = Parse_info.mk_tokens_state toks in

  let checkpoint = TH.line_of_tok tr.PI.current in

  let lexbuf_fake = Lexing.from_function (fun buf n -> raise Impossible) in

  let elems = 
    try (
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Left 
        (Common.profile_code "Parser_ml.main" (fun () ->
          if filename =~ ".*\\.mli"
          then Parser_ml.interface (lexer_function tr) lexbuf_fake
          else Parser_ml.implementation (lexer_function tr) lexbuf_fake
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

      distribute_info_items_toplevel xs toks filename, 
       stat
  | Right (info_of_bads, line_error, cur, exn) ->

      if not !Flag.error_recovery
      then raise (Parse_error (TH.info_of_tok cur));

      (match exn with
      | Lexer_ml.Lexical _ 
      | Parsing.Parse_error 
          (*| Semantic_c.Semantic _  *)
        -> ()
      | e -> raise e
      );

      if !Flag.show_parsing_error
      then 
        (match exn with
        (* Lexical is not anymore launched I think *)
        | Lexer_ml.Lexical s -> 
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

      let info_item = List.rev tr.PI.passed in
      [Ast.NotParsedCorrectly info_of_bads, info_item], 
      stat



let parse a = 
  Common.profile_code "Parse_ml.parse" (fun () -> parse2 a)

let parse_program file = 
  let (ast2, _stat) = parse file in
  program_of_program2 ast2
