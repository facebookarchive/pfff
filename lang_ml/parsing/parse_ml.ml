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
module TH   = Token_helpers_ml

module T = Parser_ml

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

type program2 = toplevel2 list
  and toplevel2 = 
    Ast.toplevel (* NotParsedCorrectly if parse error *) * info_item
     (* the token list contains also the comment-tokens *)
     and info_item = (string * Parser_ml.token list)

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

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Common.full_charpos_to_pos_large file in

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
        { ii with PI.pinfo=
          (* could assert pinfo.filename = file ? *)
               match PI.pinfo_of_info ii with
               | PI.OriginTok pi ->
                   PI.OriginTok 
                     (Common.complete_parse_info_large file table pi)
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
                   (Common.error_message file (lexbuf_to_strpos lexbuf)))
  | e -> raise e
 )
          

let tokens a = 
  Common.profile_code "Parse_ml.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Lexer tricks *)
(*****************************************************************************)

(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =

  let stat = PI.default_stat filename in

  let toks_orig = tokens filename in

  (* TODO *)
  [(), ("", toks_orig)], stat

let parse a = 
  Common.profile_code "Parse_ml.parse" (fun () -> parse2 a)

let parse_program file = 
  let (ast2, _stat) = parse file in
  program_of_program2 ast2
