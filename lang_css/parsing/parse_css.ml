(* Yoann Padioleau
 * 
 * Copyright (C) 2011 Facebook
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

module Ast = Ast_css
module Flag = Flag_parsing_css
module TH   = Token_helpers_css

module T = Parser_css

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

type program2 = Ast.stylesheet * T.token list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in

    try 
      let mltoken lexbuf = 
        Lexer_css.token lexbuf
      in
      
      let rec tokens_aux acc = 
        let tok = mltoken lexbuf in
        if !Flag.debug_lexer then Common.pr2_gen tok;

        (*
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
        *)
        
        if (match tok with T.EOF _ -> true | _ -> false)
        then List.rev (tok::acc)
        else tokens_aux (tok::acc)
      in
      tokens_aux []
  with
  | Lexer_css.Lexical s -> 
      failwith ("lexical error " ^ s ^ "\n =" ^ 
                 (PI.error_message file (PI.lexbuf_to_strpos lexbuf)))
  | e -> raise e
  )

let tokens a = 
  Common.profile_code "Parse_css.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename = 
  raise Todo

let parse a = 
  Common.profile_code "Parse_css.parse" (fun () -> parse2 a)
