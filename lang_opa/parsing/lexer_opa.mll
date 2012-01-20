{
(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module Ast = Ast_opa
module Flag = Flag_parsing_opa

open Parser_opa

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * http://doc.opalang.org/#!/manual/The-core-language
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

let error s = 
  if !Flag.error_recovery 
  then 
    if !Flag.verbose_lexing 
    then pr2 s
    else ()
  else raise (Lexical s)

let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
let keyword_table = Common.hash_of_list [
  "if", (fun ii -> Tif ii);
  "then", (fun ii -> Tthen ii);
  "else", (fun ii -> Telse ii);
  "as", (fun ii -> Tas ii);

  "match", (fun ii -> Tmatch ii);
  "case",  (fun ii -> Tcase ii); (* js syntax *)
  "default",  (fun ii -> Tdefault ii); (* js syntax *)

  "do", (fun ii -> Tdo ii); (* classic syntax *)

  "function", (fun ii -> Tfunction ii); (* js syntax *)
  "or", (fun ii -> Tor ii); (* js syntax *)

  "with", (fun ii -> Twith ii);
  "type", (fun ii -> Ttype ii);

  (* apparently accepted as regular identifiers but frowned upon *)
  "val", (fun ii -> Tval ii);
  "rec", (fun ii -> Trec ii);
  "and", (fun ii -> Tand ii);

  "begin", (fun ii -> Tbegin ii);
  "end", (fun ii -> Tend ii);

  "css", (fun ii -> Tcss ii);
  "db", (fun ii -> Tdb ii); (* classic syntax *)
  "database", (fun ii -> Tdb ii); (* js syntax *)
  "parser", (fun ii -> Tparser ii);

  "external", (fun ii -> Texternal ii);
  "forall", (fun ii -> Tforall ii);

  "package", (fun ii -> Tpackage ii);
  "import", (fun ii -> Timport ii);
  "module", (fun ii -> Tmodule ii); (* js syntax *)
  (* "xml_parser" *)

  (* js syntax *)
  "public", (fun ii -> Tpublic ii);
  "private", (fun ii -> Tprivate ii);

  "client", (fun ii -> Tclient ii);
  "server", (fun ii -> Tserver ii);

  "exposed",   (fun ii -> Texposed ii);
  "protected", (fun ii -> Tprotected ii);

  (* bool, int, float, string 
   * list, option, map, set
   * 
   * true, false
   * nil
   *)
]

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']

let newline = '\n'
let space = [' ' '\t']

let ident = (letter | '_') (letter | digit | '_')*


let nonzerodigit = ['1'-'9']
let bindigit = ['0'-'1']
let octdigit = ['0'-'7']
let hexdigit = digit | ['a'-'f'] | ['A'-'F']

(* note: most was was copied from python *)
let decimalinteger = nonzerodigit digit* | '0'
let octinteger = '0' 'o' octdigit+
let hexinteger = '0' ('x' | 'X') hexdigit+
let bininteger = '0' 'b' bindigit+

let integer = (decimalinteger | octinteger | hexinteger | bininteger)

(*****************************************************************************)
(* Main Rule *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  | "//" [^ '\n']* { TComment (tokinfo lexbuf) }

  (* less: return a different token for /** comments ? TCommentDoc? *)
  | "/*" 
      { let info = tokinfo lexbuf in 
        let com = comment lexbuf in
        TComment(info +> Parse_info.tok_add_s com) 
      }

  | newline { TCommentNewline (tokinfo lexbuf) }
  | space+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }
  (* todo? "{{" "}}" classic syntax *)

  | "." { TDot(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }

  | "->" { TArrow(tokinfo lexbuf) }
  | '_'  { TUnderscore(tokinfo lexbuf) }

  | '\\'  { TAntiSlash(tokinfo lexbuf) } (* js syntax *)

  (* operators *)
  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) }  | "/" { TDiv(tokinfo lexbuf) }
  (* | "%" { TPercent(tokinfo lexbuf) } *)

  | "="  { TEq (tokinfo lexbuf) } 
  | "=="  { TEqEq (tokinfo lexbuf) } (* could be defined as regular operator? *)
  | "!="  { TNotEq (tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TMore(tokinfo lexbuf) }
  (* | "<=" { TLessEq(tokinfo lexbuf) }  | ">=" { TMoreEq(tokinfo lexbuf) } *)

  | "?" { TQuestion(tokinfo lexbuf) }
  | "@" { TAt(tokinfo lexbuf) }
  | "#" { TSharp(tokinfo lexbuf) }

  | "&" { TAnd(tokinfo lexbuf) } | "|" { TOr(tokinfo lexbuf) }

  | "^" { THat(tokinfo lexbuf) }

  (* | "&&" { TAndAnd(tokinfo lexbuf) } *)
  | "||" { TOrOr(tokinfo lexbuf) }

  (* todo: can define operators in OPA *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ident | '`' [^ '`' '\n' '\r']+ '`' {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
      match Common.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> TIdent (s, info)
    }
  (* todo? 'a, 'b'  type variables? *)

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | integer { TInt (tok lexbuf, tokinfo lexbuf) }
  (* todo: float *)

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  (* todo: opa allows string interpolation => need a state in the lexer *)
  | '"' { 
      let info = tokinfo lexbuf in
      let s = string_double_quote lexbuf in
      TString (s, info +> Parse_info.tok_add_s (s ^ "\""))
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ { 
      error ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Comment Rule *)
(*****************************************************************************)

(* todo: OPA allow nested comments *)
and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | _  
      { let s = tok lexbuf in
        error ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment lexbuf
      }
  | eof { error "LEXER: WIERD end of file in comment"; ""}


and string_double_quote = parse
  | '"' { "" }
  (* todo: '{' *)

  | '\\' ['\\' 'n' 'r' 't' '{' '}'  '\'' '"']
      { let s = tok lexbuf in s ^ string_double_quote lexbuf }

  (* noteopti: must be the "negative" of the previous rules *)
  | [^ '{' '\\' '\"' '\n']* 
      { let s = tok lexbuf in s ^ string_double_quote lexbuf }

  | eof { error "LEXER: end of file in string_double_quote"; "'"}
  | _  { let s = tok lexbuf in
         error ("LEXER: unrecognised symbol in string_double_quote:"^s);
         s ^ string_double_quote lexbuf
    }
