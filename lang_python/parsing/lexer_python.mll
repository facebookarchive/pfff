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

(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)

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
