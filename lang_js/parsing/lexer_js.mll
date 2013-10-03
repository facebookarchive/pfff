{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

open Ast_js
open Parser_js

module Ast = Ast_js
module Flag = Flag_parsing_js

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

exception Lexical of string

let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

let error s =
  if !Flag.exn_when_lexical_error
  then raise (Lexical (s))
  else 
    if !Flag.verbose_lexing
    then pr2_once ("LEXER: " ^ s)
    else ()

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
let keyword_table = Common.hash_of_list [

  "if",         (fun ii -> T_IF ii);
  "else",       (fun ii -> T_ELSE ii);

  "while",      (fun ii -> T_WHILE ii);
  "do",         (fun ii -> T_DO ii);
  "for",        (fun ii -> T_FOR ii);

  "switch",     (fun ii -> T_SWITCH ii);
  "case",       (fun ii -> T_CASE ii);
  "default",    (fun ii -> T_DEFAULT ii);

  "break",      (fun ii -> T_BREAK ii);
  "continue",   (fun ii -> T_CONTINUE ii);

  "return",     (fun ii -> T_RETURN ii);

  "throw",      (fun ii -> T_THROW ii);
  "try",        (fun ii -> T_TRY ii);
  "catch",      (fun ii -> T_CATCH ii);

  "var",        (fun ii -> T_VAR ii);
  "function",   (fun ii -> T_FUNCTION ii);
  "const",      (fun ii -> T_CONST ii);

  "delete",     (fun ii -> T_DELETE ii);
  "new",        (fun ii -> T_NEW ii);

  "void",       (fun ii -> T_VOID ii);
  "null",       (fun ii -> T_NULL ii);

  "false",      (fun ii -> T_FALSE ii);
  "true",       (fun ii -> T_TRUE ii);

  "finally",    (fun ii -> T_FINALLY ii);
  "in",         (fun ii -> T_IN ii);
  "instanceof", (fun ii -> T_INSTANCEOF ii);
  "this",       (fun ii -> T_THIS ii);
  "typeof",     (fun ii -> T_TYPEOF ii);
  "with",       (fun ii -> T_WITH ii);

  "class",      (fun ii -> T_CLASS ii);
  "extends",    (fun ii -> T_EXTENDS ii);
  "static",     (fun ii -> T_STATIC ii);
]

(* ---------------------------------------------------------------------- *)

(* The logic to modify _last_non_whitespace_like_token is in the 
 * caller of the lexer, that is in Parse_js.tokens.
 * Used for ambiguity between / as a divisor and start of regexp.
 *)
let _last_non_whitespace_like_token = 
  ref (None: Parser_js.token option)

let reset () = 
   _last_non_whitespace_like_token := None;
  ()

} 

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)
let _WHITESPACE = [' ' '\n' '\r' '\t']+
let TABS_AND_SPACES = [' ''\t']*
let NEWLINE = ("\r"|"\n"|"\r\n")

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule initial = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" { 
      let info = tokinfo lexbuf in 
      let com = st_comment lexbuf in
      TComment(info +> PI.tok_add_s com)
    }

  | "//" {
      let info = tokinfo lexbuf in
      let com = st_one_line_comment lexbuf in
      TComment(info +> PI.tok_add_s com)
    }

  | [' ']+            { TCommentSpace(tokinfo lexbuf) }
  | ['\n' '\r' '\t']+ { TCommentNewline(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  
  | "{" { T_LCURLY (tokinfo lexbuf); }
  | "}" { T_RCURLY (tokinfo lexbuf); }

  | "(" { T_LPAREN (tokinfo lexbuf); }
  | ")" { T_RPAREN (tokinfo lexbuf); }

  | "[" { T_LBRACKET (tokinfo lexbuf); }
  | "]" { T_RBRACKET (tokinfo lexbuf); }
  | "." { T_PERIOD (tokinfo lexbuf); }
  | ";" { T_SEMICOLON (tokinfo lexbuf); }
  | "," { T_COMMA (tokinfo lexbuf); }
  | ":" { T_COLON (tokinfo lexbuf); }
  | "?" { T_PLING (tokinfo lexbuf); }
  | "&&" { T_AND (tokinfo lexbuf); }
  | "||" { T_OR (tokinfo lexbuf); }
  | "===" { T_STRICT_EQUAL (tokinfo lexbuf); }
  | "!==" { T_STRICT_NOT_EQUAL (tokinfo lexbuf); }
  | "<=" { T_LESS_THAN_EQUAL (tokinfo lexbuf); }
  | ">=" { T_GREATER_THAN_EQUAL (tokinfo lexbuf); }
  | "==" { T_EQUAL (tokinfo lexbuf); }
  | "!=" { T_NOT_EQUAL (tokinfo lexbuf); }
  | "++" { T_INCR (tokinfo lexbuf); }
  | "--" { T_DECR (tokinfo lexbuf); }
  | "<<=" { T_LSHIFT_ASSIGN (tokinfo lexbuf); }
  | "<<" { T_LSHIFT (tokinfo lexbuf); }
  | ">>=" { T_RSHIFT_ASSIGN (tokinfo lexbuf); }
  | ">>>=" { T_RSHIFT3_ASSIGN (tokinfo lexbuf); }
  | ">>>" { T_RSHIFT3 (tokinfo lexbuf); }
  | ">>" { T_RSHIFT (tokinfo lexbuf); }
  | "+=" { T_PLUS_ASSIGN (tokinfo lexbuf); }
  | "-=" { T_MINUS_ASSIGN (tokinfo lexbuf); }

  | "*=" { T_MULT_ASSIGN (tokinfo lexbuf); }
  | "%=" { T_MOD_ASSIGN (tokinfo lexbuf); }
  | "&=" { T_BIT_AND_ASSIGN (tokinfo lexbuf); }
  | "|=" { T_BIT_OR_ASSIGN (tokinfo lexbuf); }
  | "^=" { T_BIT_XOR_ASSIGN (tokinfo lexbuf); }
  | "<" { T_LESS_THAN (tokinfo lexbuf); }
  | ">" { T_GREATER_THAN (tokinfo lexbuf); }
  | "+" { T_PLUS (tokinfo lexbuf); }
  | "-" { T_MINUS (tokinfo lexbuf); }
  | "*" { T_MULT (tokinfo lexbuf); }
  (* for '/' see below the regexp handling *)
  | "%" { T_MOD (tokinfo lexbuf); }
  | "|" { T_BIT_OR (tokinfo lexbuf); }
  | "&" { T_BIT_AND (tokinfo lexbuf); }
  | "^" { T_BIT_XOR (tokinfo lexbuf); }
  | "!" { T_NOT (tokinfo lexbuf); }
  | "~" { T_BIT_NOT (tokinfo lexbuf); }
  | "=" { T_ASSIGN (tokinfo lexbuf); }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''0'-'9']* {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      match Common2.optionise (fun () -> 
        Hashtbl.find keyword_table s (* need case insensitive ? *))
      with
      | Some f -> f info
      | None -> T_IDENTIFIER (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | "0x"['a'-'f''A'-'F''0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }
  | '0'['0'-'7']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }

  | ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+ (* {1,3} *) {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }

  | ['0'-'9']+'.'? |
    ['0'-'9']*'.'['0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | "'" { 
      let info = tokinfo lexbuf in 
      let s = string_quote lexbuf in
      (* s does not contain the enclosing "'" but the info does *)
      T_STRING (s, info +> PI.tok_add_s (s ^ "'"))
    }

  | '"' { 
      let info = tokinfo lexbuf in
      let s = string_double_quote lexbuf in 
      T_STRING (s, info +> PI.tok_add_s (s ^ "\""))
    }

  (* ----------------------------------------------------------------------- *)
  (* Regexp *)
  (* ----------------------------------------------------------------------- *)
  (* take care of ambiguity with start of comment //, and with 
   * '/' as a divisor operator
   *
   * it can not be '/' [^ '/']* '/' because then
   * comments will not be recognized as lex tries
   * to find the longest match.
   * 
   * It can not be 
   * '/' [^'*''/'] ([^'/''\n'])* '/' ['A'-'Z''a'-'z']*
   * because a / (b/c)  will be recognized as a regexp.
   * 
   *)

  (* todo? marcel was changing of state context condition there *)
  | "/=" { T_DIV_ASSIGN (tokinfo lexbuf); }

  | "/" { 
      let info = tokinfo lexbuf in 

      match !_last_non_whitespace_like_token with
      | Some (
            T_IDENTIFIER _
          | T_NUMBER _
          | T_STRING _
          | T_REGEX _
          | T_INCR _ | T_DECR _
          | T_RBRACKET _ 
          | T_RPAREN _
          | T_FALSE _ | T_TRUE _
          | T_NULL _
          | T_THIS _
        ) -> 
          T_DIV (info); 
      | _ ->
          let s = regexp lexbuf in 
          T_REGEX ("/" ^ s, info +> PI.tok_add_s (s))
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ { 
      error ("unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule string *)
(*****************************************************************************)
and string_quote = parse
  | "'"            { "" }
  | (_ as x)       { Common2.string_of_char x^string_quote lexbuf}
  | ("\\" (_ as v)) as x { 
      (* check char ? *)
      (match v with
      | _ -> ()
      );
      x ^ string_quote lexbuf
    }
  | eof { error "WIERD end of file in quoted string"; ""}

and string_double_quote  = parse
  | '"'            { "" }
  | (_ as x)       { Common2.string_of_char x^string_double_quote lexbuf}
  | ("\\" (_ as v)) as x { 
      (* check char ? *)
      (match v with
      | _ -> ()
      );
      x ^ string_double_quote lexbuf
    }
  | eof { error "WIERD end of file in double quoted string"; ""}

(*****************************************************************************)
(* Rule regexp *)
(*****************************************************************************)
and regexp = parse
  | '/'            { "/" ^ regexp_maybe_ident lexbuf }
  | (_ as x)       { Common2.string_of_char x^regexp lexbuf}
  | ("\\" (_ as v)) as x { 
      (* check char ? *)
      (match v with
      | _ -> ()
      );
      x ^ regexp lexbuf
    }
  | eof { error "WIERD end of file in regexp"; ""}

and regexp_maybe_ident = parse
  | ['A'-'Z''a'-'z']* { tok lexbuf }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)

and st_comment = parse 
  | "*/" { tok lexbuf }

  (* noteopti: *)
  | [^'*']+ { let s = tok lexbuf in s ^ st_comment lexbuf } 
  | "*"     { let s = tok lexbuf in s ^ st_comment lexbuf }

  | eof { error "end of file in comment"; "*/"}
  | _  { 
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s);
      s ^ st_comment lexbuf
    }

and st_one_line_comment = parse
  | [^'\n' '\r']* { 
      let s = tok lexbuf in
      s ^ st_one_line_comment lexbuf
    }

  | NEWLINE { tok lexbuf }

  | eof { error "end of file in comment"; "\n" }
  | _ { 
    error ("unrecognised symbol, in st_one_line_comment rule:"^tok lexbuf);
    tok lexbuf
    }
