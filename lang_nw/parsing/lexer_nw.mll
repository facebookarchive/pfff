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
 *  - heavea, but the code is quite complicated. I don't need all the
 *    features of TeX
 *  - extend the parser in syncweb, but it's not a parser. It is just
 *    a very specialized lexer that recognize only noweb constructs
 *)

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

(* ---------------------------------------------------------------------- *)
(* Keywords part 1 *)
(* ---------------------------------------------------------------------- *)

(*
let keyword_table = Common.hash_of_list [
]
*)

}

(*****************************************************************************)

(*****************************************************************************)
rule tex = parse
  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)
  (* ----------------------------------------------------------------------- *)
    | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }

(*****************************************************************************)
and noweb = parse
  (* ----------------------------------------------------------------------- *)
    | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in noweb rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }


(*****************************************************************************)
and verbatim endname = parse
  (* ----------------------------------------------------------------------- *)
    | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
    | _ { 
        if !Flag.verbose_lexing 
        then pr2_once ("LEXER:unrecognised symbol, in verbatim rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }
