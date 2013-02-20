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

module Ast = Ast_python
module Flag = Flag_parsing_python

open Parser_python


(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * src: http://inst.eecs.berkeley.edu/~cs164/sp10/python-grammar.html
 * which was itself from the python reference manual at:
 * http://docs.python.org/release/2.5.4/ref/ref.html
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

let keyword_table = Common.hash_of_list [
  "and",      (fun ii -> Tand ii);     
  "del",      (fun ii -> Tdel ii);     
  "from",     (fun ii -> Tfrom ii);
  "not",      (fun ii -> Tnot ii);
  "while",    (fun ii -> Twhile ii);    
  "as",       (fun ii -> Tas ii);
  "elif",     (fun ii -> Telif ii);
  "global",   (fun ii -> Tglobal ii);
  "or",       (fun ii -> Tor ii);
  "with",     (fun ii -> Twith ii);     
  "assert",   (fun ii -> Tassert ii);
  "else",     (fun ii -> Telse ii);
  "if",       (fun ii -> Tif ii);
  "pass",     (fun ii -> Tpass ii);
  "yield",    (fun ii -> Tyield ii);    
  "break",    (fun ii -> Tbreak ii);
  "except",   (fun ii -> Texcept ii);
  "import",   (fun ii -> Timport ii);
  "print",    (fun ii -> Tprint ii);
  "class",    (fun ii -> Tclass ii);
  "exec",     (fun ii -> Texec ii);
  "in",       (fun ii -> Tin ii);
  "raise",    (fun ii -> Traise ii);              
  "continue", (fun ii -> Tcontinue ii);
  "finally",  (fun ii -> Tfinally ii);
  "is",       (fun ii -> Tis ii);
  "return",   (fun ii -> Treturn ii);             
  "def",      (fun ii -> Tdef ii);
  "for",      (fun ii -> Tfor ii);
  "lambda",   (fun ii -> Tlambda ii);
  "try",      (fun ii -> Ttry ii);
]


}
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']

let ident = (letter | '_') (letter | digit | '_')*

let newline = '\n'
let space = [' ' '\t']

let nonzerodigit = ['1'-'9']
let octdigit = ['0'-'7']
let hexdigit = digit | ['a'-'f'] | ['A'-'F']

let octal = ['0'-'7']

(* in long string we can have any kind of \, like \[, so '\\' ['a'-'z'] is
 * not enough
 *)
let escapeseq = 
   ( '\\' octal octal octal | '\\' _)

let decimalinteger = nonzerodigit digit* | '0'
let octinteger = '0' octdigit+
let hexinteger = '0' ('x' | 'X') hexdigit+

let integer = (decimalinteger | octinteger | hexinteger)

let intpart = digit+
let fraction = '.' digit+
let exponent = ('e' | 'E') ('+' | '-')? digit+

let pointfloat = intpart? fraction | intpart '.'
let exponentfloat = (intpart | pointfloat) exponent

let floatnumber = (pointfloat | exponentfloat)

let imagnumber = (floatnumber | intpart) ('j' | 'J')

(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "#" [^ '\n']* { TComment (tokinfo lexbuf) }

  (* python use haskell layout so newline and indentation has to be handled
   * in a special way by the caller of the lexer
   *)
  | newline { TCommentNewline (tokinfo lexbuf) }
  | space+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  | "="  { TEq (tokinfo lexbuf) }

  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }
  | "<<" { TOAngle(tokinfo lexbuf) }  | ">>" { TCAngle(tokinfo lexbuf) }

  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TMore(tokinfo lexbuf) }
  | "<=" { TLessEq(tokinfo lexbuf) }  | ">=" { TMoreEq(tokinfo lexbuf) }


  | "==" { TEqEq(tokinfo lexbuf) }
  | "<>" { TDiff(tokinfo lexbuf) }
  | "!=" { TNotEq(tokinfo lexbuf) }

  | "&" { TAnd(tokinfo lexbuf) }
  | "|" { TOr(tokinfo lexbuf) }
  | "^" { TXor(tokinfo lexbuf) }

  | "`" { TBackQuote(tokinfo lexbuf) }
  | "@" { TAt(tokinfo lexbuf) }

  | "*" { TStar(tokinfo lexbuf) }
  | "**" { TStarStar(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }

  | "." { TDot(tokinfo lexbuf) }
  | "..." { TEllipsis(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | "~" { TTilde(tokinfo lexbuf) }

  | "/" { TSlash(tokinfo lexbuf) }
  | "//" { TSlashSlash(tokinfo lexbuf) }
  | "%" { TPercent(tokinfo lexbuf) }


  | "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "**="
  | ">>=" | "<<=" | "&=" | "^=" | "|=" { 
      let s = tok lexbuf in
      TAugOp (s, tokinfo lexbuf)
    }
          

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ident {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> TIdent (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | integer             { TInt (tok lexbuf, tokinfo lexbuf) }
  | integer ('l' | 'L') { TInt (tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)

  | ['u''U']? "'" { 
      let info = tokinfo lexbuf in
      let s = string_quote lexbuf in
      TString (s, info +> Parse_info.tok_add_s (s ^ "'"))
    }
  | ['u''U']? '"' { 
      let info = tokinfo lexbuf in
      let s = string_double_quote lexbuf in
      TString (s, info +> Parse_info.tok_add_s (s ^ "\""))
    }

  | ['u''U']? "'''" { 
      let info = tokinfo lexbuf in
      let s = string_triple_quote lexbuf in
      TLongString (s, info +> Parse_info.tok_add_s (s ^ "'''"))
    }
  | ['u''U']? '"' '"' '"' { 
      let info = tokinfo lexbuf in
      let s = string_triple_double_quote lexbuf in
      TLongString (s, info +> Parse_info.tok_add_s (s ^ "\"\"\""))
    }

  (* TODO: the rules for the raw string are not exactly the same;
   * should not call the same string_xxx
   *)
  | ("r" | "ur" | "R" | "UR" | "Ur" | "uR") "'" {
      let info = tokinfo lexbuf in
      let s = string_quote lexbuf in
      TString (s, info +> Parse_info.tok_add_s (s ^ "'"))
    }
  | ("r" | "ur" | "R" | "UR" | "Ur" | "uR") '"' {
      let info = tokinfo lexbuf in
      let s = string_double_quote lexbuf in
      TString (s, info +> Parse_info.tok_add_s (s ^ "'"))
    }

  | ("r" | "ur" | "R" | "UR" | "Ur" | "uR") "'''" {
      let info = tokinfo lexbuf in
      let s = string_triple_quote lexbuf in
      TLongString (s, info +> Parse_info.tok_add_s (s ^ "'"))
    }
  | ("r" | "ur" | "R" | "UR" | "Ur" | "uR") '"' '"' '"' {
      let info = tokinfo lexbuf in
      let s = string_triple_double_quote lexbuf in
      TLongString (s, info +> Parse_info.tok_add_s (s ^ "'"))
    }
    

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)

and string_quote = parse
  | "'" { "" }

  | [^ '\'' '\\' '\n']* { let s = tok lexbuf in s ^ string_quote lexbuf }
  | escapeseq { let s = tok lexbuf in s ^ string_quote lexbuf }

  | eof { pr2 "LEXER: end of file in string_quote"; "'"}
  | _  { let s = tok lexbuf in
         pr2 ("LEXER: unrecognised symbol in string_quote:"^s);
         s ^ string_quote lexbuf
    }

and string_double_quote = parse
  | '"' { "" }

  | [^ '\"' '\n']* { let s = tok lexbuf in s ^ string_double_quote lexbuf }
  | escapeseq { let s = tok lexbuf in s ^ string_double_quote lexbuf }


  | eof { pr2 "LEXER: end of file in string_double_quote"; "'"}
  | _  { let s = tok lexbuf in
         pr2 ("LEXER: unrecognised symbol in string_double_quote:"^s);
         s ^ string_double_quote lexbuf
    }

and string_triple_quote = parse
  | "'''" { "" }

  | [^ '\\' '\'' ]* { let s = tok lexbuf in s ^ string_triple_quote lexbuf }
  | escapeseq { let s = tok lexbuf in s ^ string_triple_quote lexbuf }
  | "'" { let s = tok lexbuf in s ^ string_triple_quote lexbuf }

  | eof { pr2 "LEXER: end of file in string_triple_quote"; "'"}
  | _  { let s = tok lexbuf in
         pr2 ("LEXER: unrecognised symbol in string_triple_quote:"^s);
         s ^ string_triple_quote lexbuf
    }

and string_triple_double_quote = parse
  | '"' '"' '"' { "" }

  | [^ '\\' '"' ]* { let s = tok lexbuf in s ^ string_triple_double_quote lexbuf }
  | escapeseq { let s = tok lexbuf in s ^ string_triple_double_quote lexbuf }
  | '"' { let s = tok lexbuf in s ^ string_triple_double_quote lexbuf }

  | eof { pr2 "LEXER: end of file in string_triple_double_quote"; "'"}
  | _  { let s = tok lexbuf in
         pr2 ("LEXER: unrecognised symbol in string_triple_double_quote:"^s);
         s ^ string_triple_double_quote lexbuf
    }
