{
(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

open Parser_clang
module Flag = Flag_parsing_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

exception Lexical of string

let tok     lexbuf  = 
  Lexing.lexeme lexbuf

}
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']

let newline = '\n'
let space = [' ' '\t']

let hexdigit = digit | ['a'-'f'] | ['A'-'F']

let hexinteger = '0' ('x' | 'X') hexdigit+

(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  | newline { token lexbuf }
  | space+ { token lexbuf }

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
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF }

  | _ { 
      if !Flag.verbose_lexing 
      then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tok lexbuf)
    }

(*****************************************************************************)
