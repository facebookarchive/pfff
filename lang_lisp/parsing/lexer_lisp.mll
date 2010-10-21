{
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

module Flag = Flag_parsing_lisp

open Parser_lisp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_lexing 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

(* ---------------------------------------------------------------------- *)
let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

}

(*****************************************************************************)

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']

(*****************************************************************************)
rule sexp = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | ";" [^'\n' '\r']* { 
      TComment(tokinfo lexbuf)
    }
  | [' ''\t'] { TCommentSpace (tokinfo lexbuf) }
  | "\n" { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)

  | '(' { TOParen (tokinfo lexbuf) }
  | ')' { TCParen (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | digit+ {
      TNumber(tok lexbuf, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
    }
