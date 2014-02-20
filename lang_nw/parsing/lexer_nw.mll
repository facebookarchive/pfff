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

module Flag = Flag_parsing_nw
open Parser_nw

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Alternatives:
 *  - hevea, but the code is quite complicated. I don't need all the
 *    features of TeX
 *  - extend the parser in syncweb, but it's not a parser. It is just
 *    a very specialized lexer that recognizes only noweb constructs
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_lexing 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

(* ---------------------------------------------------------------------- *)
let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(* ---------------------------------------------------------------------- *)
(* Keywords part 1 *)
(* ---------------------------------------------------------------------- *)

(*
let keyword_table = Common.hash_of_list [
]
*)

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
type state_mode = 
  (* aka TeX mode *)
  | INITIAL
  (* started with begin{verbatim} (or variant), finished end{verbatim} *)
  | IN_VERBATIM of string
  (* started with <<xxx>>= *)
  | IN_NOWEB_CHUNK

let default_state = INITIAL

let _mode_stack = 
  ref [default_state]

let reset () = 
  _mode_stack := [default_state];
  ()

let rec current_mode () = 
  try 
    Common2.top !_mode_stack
  with Failure("hd") -> 
    pr2("LEXER: mode_stack is empty, defaulting to INITIAL");
    reset();
    current_mode ()

let push_mode mode = Common.push mode _mode_stack
let pop_mode () = ignore(Common2.pop2 _mode_stack)

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']

(*****************************************************************************)
(* Rule in Tex *)
(*****************************************************************************)
rule tex = parse
  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "%" [^'\n' '\r']* { 
      TComment(tokinfo lexbuf)
    }
  (* actually in lex space and newlines have meaning so should perhaps
   * rename those tokens
   *)
  | [' ''\t'] { TCommentSpace (tokinfo lexbuf) }
  | "\n" { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | "{" { TOBrace (tokinfo lexbuf); }
  | "}" { TCBrace (tokinfo lexbuf); }

  | '[' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | ']' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | '(' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | ')' { TSymbol (tok lexbuf, tokinfo lexbuf) }

  | ['-' '+' '=' '\'' '\\' '.' '@' ',' '/' ':' '<' '>' '*' ';' '#' '"'
     '_' '`' '?' '^' '|' '!' '&' ]+ {
      TSymbol (tok lexbuf, tokinfo lexbuf) 
    }
  (* don't want ~\foo to be tokenized as ~\ *)
  | "~" { TSymbol (tok lexbuf, tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | "\\" ((letter+) as cmd) { 
      TCommand (cmd, tokinfo lexbuf)
    }

  | letter+ {
      TWord(tok lexbuf, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | digit+ {
      TNumber(tok lexbuf, tokinfo lexbuf)
    }
  (* ----------------------------------------------------------------------- *)
  (* Noweb *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* Special modes *)
  (* ----------------------------------------------------------------------- *)
  | "\\begin{verbatim}"
      {
        push_mode (IN_VERBATIM ("verbatim"));
        TBeginVerbatim (tokinfo lexbuf)
      }

  | "<<" ([^'>']+ as _tagname) ">>=" {
      push_mode IN_NOWEB_CHUNK;
      TBeginNowebChunk (tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule in Code noweb *)
(*****************************************************************************)
and noweb = parse
  | "\n@" { 
      pop_mode ();
      TEndNowebChunk (tokinfo lexbuf)
    }
  | ([^'\n']+ as line) { TNowebChunkLine (line, tokinfo lexbuf) }
  | '\n' { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in noweb rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }


(*****************************************************************************)
(* Rule in verbatim *)
(*****************************************************************************)
and verbatim endname = parse
  | "\\end{verbatim}" { 
      pop_mode ();
      TEndVerbatim (tokinfo lexbuf)
    }
  (* note: if end{verbatim} is not alone on its line then
   * this regexp will take precedence because of the longest-match
   * behavior of lex. So keep \end{verabatim} alone on its line!
   *)
  | ([^'\n']+ as line) { TVerbatimLine (line, tokinfo lexbuf) }
  | '\n' { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in verbatim rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }
