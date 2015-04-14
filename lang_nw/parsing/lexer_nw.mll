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
(* 
 * Alternatives:
 *  - hevea, but the code is quite complicated. I don't need all the
 *    features of TeX
 *  - extend the parser in syncweb, but it's not a parser. It is just
 *    a very specialized lexer that recognizes only noweb constructs
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* was in parser_nw.mly but we don't really need an extra file *)

type info = Ast_nw.info

type token =
  | TComment of info
  | TCommentSpace of info
  | TCommentNewline of info

  | TCommand of (string * info)
  | TWord of (string * info)
  | TSymbol of (string * info)
  | TNumber of (string * info)

  | TBeginVerbatim of info | TEndVerbatim of info
  | TVerbatimLine of (string * info)

  | TBeginNowebChunk of info | TEndNowebChunk of info
  | TNowebChunkLine of (string * info)
  | TBeginNowebChunkName of info | TEndNowebChunkName of info
  | TNowebChunkName of info | TNowebAngle of info

  | TOBrace of info
  | TCBrace of info
  | TUnknown of info
  | EOF of info

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
  (* started with << *)
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
  | "<<" {
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
  | ([^'\n''>']+) { TNowebChunkName (tokinfo lexbuf) }
  | '>' { TNowebChunkName (tokinfo lexbuf) }

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
