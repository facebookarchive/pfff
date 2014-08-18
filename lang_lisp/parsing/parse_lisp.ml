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

module Ast = Ast_lisp
module Flag = Flag_parsing_lisp
module PI = Parse_info
(* we don't need a full grammar for lisp code, so we put everything,
 * the token type, the helper in parser_ml. No token_helpers_lisp.ml
 *)
module TH = Parser_lisp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * alt: 
 *  - Could reuse the parser in ocamlsexp ? but they just have Atom | Sexp
 *    and I need to differentiate numbers in the highlighter, and
 *    also handling quoted, anti-quoted and other lisp special things.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_lisp.toplevel option * Parser_lisp.token list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let lexbuf_to_strpos lexbuf     = 
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* could factorize and take the tokenf and visitor_of_infof in argument
 * but sometimes copy-paste is ok.
 *)
let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    try 
      let ftoken lexbuf = 
        Lexer_lisp.token lexbuf
      in
      
      let rec tokens_aux acc = 
        let tok = ftoken lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

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
  | Lexer_lisp.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                 (PI.error_message file (lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )

let tokens a = 
  Common.profile_code "Parse_lisp.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =

  let stat = Parse_info.default_stat filename in
  let toks_orig = tokens filename in

  let ast = None in
  (ast, toks_orig), stat

let parse a = 
  Common.profile_code "Parse_lisp.parse" (fun () -> parse2 a)
