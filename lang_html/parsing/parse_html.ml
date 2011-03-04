(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

module Ast = Ast_html
module Flag = Flag_parsing_html
module TH   = Token_helpers_html

module T = Parser_html

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Lots of copy paste with my other parsers (e.g. PHP, C, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  let table     = Parse_info.full_charpos_to_pos_large file in
  raise Todo

let tokens a = 
  Common.profile_code "Parse_html.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Alternate parsers *)
(*****************************************************************************)

(* a small wrapper over ocamlnet *)
let (parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2) = 
 fun (Ast.HtmlRaw raw) -> 
  let ch = new Netchannels.input_string raw in
  Nethtml.parse ch

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =
  raise Todo


let parse a = 
  Common.profile_code "Parse_html.parse" (fun () -> parse2 a)

