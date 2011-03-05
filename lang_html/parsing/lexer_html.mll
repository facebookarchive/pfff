{
(* Yoann Padioleau
 *
 * Copyright (C) 2001-2006 Patrick Doane and Gerd Stolpmann
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

open Ast_html

module Ast = Ast_html
module Flag = Flag_parsing_html

open Parser_html

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * src: most of the code in this file comes from ocamlnet/netstring/.
 * The original CVS ID is:
 * $Id: nethtml_scanner.mll 1219 2009-04-14 13:28:56Z ChriS $
 * I've extended it mainly to add position information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(* ---------------------------------------------------------------------- *)
}

(*****************************************************************************)

(* Simplified rules: Only ASCII is recognized as character set *)

let letter = ['A'-'Z' 'a'-'z' ]
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let ws = [ ' ' '\t' '\r' '\n' ]

let namechar = letter | digit | '.' | ':' | '-' | '_'
let name = ( letter | '_' | ':' ) namechar*
let nmtoken = namechar+

let string_literal1 = '"' [^ '"' ]* '"'
let string_literal2 = "'" [^ '\'' ]* "'"
let string_literal3 = [^ '"' '\'' '>' '=' ' ' '\t' '\n' '\r' ]+
let string_literal4 = [^ '"' '\'' '>' ' ' '\t' '\n' '\r' ]+

(*****************************************************************************)

(* This following rules reflect HTML as it is used, not the SGML rules. *)
rule scan_document = parse
  | "<!--"      { Lcomment(tokinfo lexbuf) }

  | "<!"        { Ldoctype(tokinfo lexbuf) }
  | "<?"        { Lpi(tokinfo lexbuf) }

  | "<" (name as s)  { Lelement (tokinfo lexbuf, s) }
  | "</" (name as s) { Lelementend (tokinfo lexbuf, s) }
  (* todo? parse error ? *)
  | "<" (* misplaced "<" *) { Cdata (tokinfo lexbuf, "<") }
  | [^ '<' ]+               { Cdata (tokinfo lexbuf, tok lexbuf) }

  | eof { EOF (tokinfo lexbuf) }

(*****************************************************************************)
and scan_special = parse
  | "</" (name as s) { Lelementend (tokinfo lexbuf, s) }
  | "<"              { Cdata (tokinfo lexbuf, "<") }
  | [^ '<' ]+        { Cdata (tokinfo lexbuf, tok lexbuf) }

  | eof { EOF (tokinfo lexbuf) }

(*****************************************************************************)
and scan_comment = parse
  (* FIXME: There may be any number of ws between -- and > *)
  | "-->"  { Rcomment (tokinfo lexbuf) } 
  | "-"    { Mcomment (tokinfo lexbuf) }
  | [^ '-']+  { Mcomment (tokinfo lexbuf) }

  | eof { EOF (tokinfo lexbuf) }

(*****************************************************************************)
and scan_doctype = parse
  (* Occurence in strings, and [ ] brackets ignored *)
  | ">"         { Rdoctype (tokinfo lexbuf) }
  | [^ '>' ]+   { Mdoctype (tokinfo lexbuf) }

  | eof { EOF (tokinfo lexbuf) }

and scan_pi = parse
  | "?>"   { Rpi (tokinfo lexbuf) }
  | ">"    { Rpi (tokinfo lexbuf) }
  | '?'    { Mpi (tokinfo lexbuf)  }
  | [^ '>' '?' ]+ { Mpi (tokinfo lexbuf) }
  | eof { EOF (tokinfo lexbuf) }

(*****************************************************************************)
and scan_element = parse
  | ">"     { Relement (tokinfo lexbuf) }
  | "/>"    { Relement_empty (tokinfo lexbuf) }
  | ws+     { Space (tokinfo lexbuf, tok lexbuf) }
  | name    { Name (tokinfo lexbuf, tok lexbuf) }
  | "="     { Is (tokinfo lexbuf) }
  | '"'     { Other (tokinfo lexbuf) }
  | "'"     { Other (tokinfo lexbuf) }
  | string_literal3 { Literal (tokinfo lexbuf, tok lexbuf) }
  | _       { Other (tokinfo lexbuf) }
  | eof     { EOF (tokinfo lexbuf) }

and scan_element_after_Is = parse
  | ">"     { Relement (tokinfo lexbuf) }
  | "/>"    { Relement_empty (tokinfo lexbuf) }
  | ws+     { Space (tokinfo lexbuf, tok lexbuf) }
  | '"'    ( [^ '"' ]* as s) '"' 
      {  Literal (tokinfo lexbuf, s) }
  | '"' { Other (tokinfo lexbuf) }
  | "'" ( [^ '\'' ]* as s) '\''
      { Literal (tokinfo lexbuf, s) }
  | "'" { Other (tokinfo lexbuf) }

  | string_literal4 { Literal ((tokinfo lexbuf), tok lexbuf) }
  | _               { Other (tokinfo lexbuf) }
  | eof             { EOF (tokinfo lexbuf) }

