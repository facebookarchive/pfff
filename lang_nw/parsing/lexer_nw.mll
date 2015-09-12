{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2015 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A basic Tex/LaTeX/Noweb lexer. 
 *
 * Alternatives:
 *  - hevea, but the code is quite complicated. I don't need all the
 *    features of TeX
 *  - extend the parser in syncweb, but it's not a parser. It is just
 *    a very specialized lexer that recognizes only noweb constructs
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* Was in parser_nw.mly but we don't really need an extra file. The
 * only "parsing" we could do is just to make a fuzzy AST by parentizing
 * braces.
 *)

type token =
  | TComment of Parse_info.info
  | TCommentSpace of Parse_info.info
  | TCommentNewline of Parse_info.info

  | TWord of (string * Parse_info.info)
  | TNumber of (string * Parse_info.info)
  | TSymbol of (string * Parse_info.info)

  (* \xxx *)
  | TCommand of (string * Parse_info.info)
  | TOBrace of Parse_info.info | TCBrace of Parse_info.info
  (* no TOParen/TCParen, they are not forced to be matching in TeX *)
  (* pad-specific: \t \f \l, see noweblatexpad  *)
  | TFootnote of char * Parse_info.info

  (* verbatim (different lexing rules) *)

  | TBeginVerbatim of Parse_info.info 
  | TEndVerbatim of Parse_info.info
  | TVerbatimLine of (string * Parse_info.info)

  (* start of noweb stuff (different lexing rules too) *)

  (* <<...>>= and @ *)
  | TBeginNowebChunk of Parse_info.info 
  | TEndNowebChunk of Parse_info.info
  | TNowebChunkLine of (string * Parse_info.info)

  (* << and >> when on the same line and inside a noweb chunk *)
  | TBeginNowebChunkName of Parse_info.info 
  | TEndNowebChunkName of Parse_info.info

  | TNowebChunkName of Parse_info.info 
  | TNowebAngle of Parse_info.info


  | TUnknown of Parse_info.info
  | EOF of Parse_info.info

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_lexing 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

let error s =
  if !Flag.verbose_lexing
  then pr2_once ("LEXER: " ^ s)

(* pad: hack around ocamllex to emulate the yyless() of flex. The semantic
 * is not exactly the same than yyless(), so I use yyback() instead.
 * http://my.safaribooksonline.com/book/programming/flex/9780596805418/a-reference-for-flex-specifications/yyless
 *)
let yyback n lexbuf =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }

(* ---------------------------------------------------------------------- *)
let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(* let keyword_table = Common.hash_of_list [] ? not needed. No keyword
 * in Tex, just commands.
 *)

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
type state_mode = 
  (* aka TeX mode *)
  | INITIAL
  (* started with begin{verbatim} (or variant), finished by end{verbatim} *)
  | IN_VERBATIM of string
  (* started with <<xxx>>=, finished by @ *)
  | IN_NOWEB_CHUNK
  (* started with <<, finished by >> when they are on the same line *)
  | IN_NOWEB_CHUNKNAME

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
let pop_mode () = Common2.pop2 _mode_stack |> ignore

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
  (* Actually in Tex the space and newlines have a meaning so I should perhaps
   * rename those tokens.
   *)
  | [' ''\t'] { TCommentSpace (tokinfo lexbuf) }
  | "\n" { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | "{" { TOBrace (tokinfo lexbuf); }
  | "}" { TCBrace (tokinfo lexbuf); }

  (* they don't have to be matching in TeX, so no need for special tokens *)
  | '[' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | ']' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | '(' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | ')' { TSymbol (tok lexbuf, tokinfo lexbuf) }

  (* don't want ~\foo to be tokenized as ~\ *)
  | "~" { TSymbol (tok lexbuf, tokinfo lexbuf) }

  | ['-' '+' '=' '\'' '\\' '.' '@' ',' '/' ':' '<' '>' '*' ';' '#' '"'
     '_' '`' '?' '^' '|' '!' '&' ]+ {
      TSymbol (tok lexbuf, tokinfo lexbuf) 
    }


  (* ----------------------------------------------------------------------- *)
  (* Commands and words (=~ Keywords and indent in other PL) *)
  (* ----------------------------------------------------------------------- *)
  (* very pad-specific, for noweblatexpad, todo, less, note shortcuts *)
  | "\\" (['t''l''n'] as kind) [' ''\t'] [^'\n' '\r']*
      { TFootnote (kind, tokinfo lexbuf) }

  | "\\" ((letter+) as cmd) { TCommand (cmd, tokinfo lexbuf)}
  | letter+ { TWord(tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | digit+ { TNumber(tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Noweb *)
  (* ----------------------------------------------------------------------- *)

  | "<<" ([^'>']+ as _tagname) ">>=" {
      push_mode IN_NOWEB_CHUNK;
      TBeginNowebChunk (tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Special modes *)
  (* ----------------------------------------------------------------------- *)
  | "\\begin{verbatim}"
      {
        push_mode (IN_VERBATIM ("verbatim"));
        TBeginVerbatim (tokinfo lexbuf)
      }


  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
        error ("unrecognised symbol, in token rule:"^tok lexbuf);
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
  (* bugfix: they have to be on the same line! otherwise C code using
   * << will screw the parsing
   *)
  | "<<" ([^'\n' '\r']+ as name) ">>" {
      yyback (String.length name + 2) lexbuf;
      push_mode IN_NOWEB_CHUNKNAME;
      TBeginNowebChunkName (tokinfo lexbuf);
    }
  | ([^'\n''<']+ as line) { TNowebChunkLine (line, tokinfo lexbuf) }
  | '\n' { TCommentNewline (tokinfo lexbuf) }
  | '<'  { TNowebAngle (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
      error ("unrecognised symbol, in noweb chunkname rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule in noweb chunkname *)
(*****************************************************************************)
and noweb_chunkname = parse
  | ">>" { 
      pop_mode ();
      TEndNowebChunkName (tokinfo lexbuf)
    }
  | ([^'>']+) { TNowebChunkName (tokinfo lexbuf) }
  | '>' { TNowebChunkName (tokinfo lexbuf) }
  | '\n' { 
         error ("impossible, should not get newline in chunkname");
         TUnknown (tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _ { 
      error ("unrecognised symbol, in noweb rule:"^tok lexbuf);
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
      error ("unrecognised symbol, in verbatim rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }
