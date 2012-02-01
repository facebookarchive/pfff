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
 *
 * There are a few tricks to go around ocamllex restrictions
 * because OPA has different lexing rules depending on the "context"
 * like PHP, Perl, etc.
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

(* pad: hack around ocamllex to emulate the yyless feature of flex *)
let yyless n lexbuf = 
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with 
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }

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

  "int", (fun ii -> Tint ii);
  "float", (fun ii -> Tfloat ii);
  "string", (fun ii -> Tstring ii);
  (* bool
   * list, option, map, set
   * 
   * true, false
   * nil
   *)
]

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
(* this is similar to what we do in lexer_php.mll, see the doc there *)
type state_mode = 
  (* initial mode, also started with '{' inside strings or html
   * and terminated by '}'
   *)
  | ST_INITIAL
  (* started with ", finished with ". In most languages strings 
   * are a single tokens but OPA allows interpolation which means 
   * a string can contain nested OPA expressions.
   *)
  | ST_DOUBLE_QUOTES
  (* started with <xx when preceded by a certain token (e.g. '='),
   * finished by '>' by transiting to ST_IN_XML_TEXT, or really finished
   * by '/>'.
   *)
  | ST_IN_XML_TAG of Ast_opa.tag (* the current tag, e,g, "span" *)
  (* started with the '>' of an opening tag, finished when '</x>' *)
  | ST_IN_XML_TEXT of Ast_opa.tag (* the current tag *)

let default_state = ST_INITIAL
let _mode_stack = ref []
(* The logic to modify _last_non_whitespace_like_token is in the 
 * caller of the lexer, that is in Parse_opa.tokens.
 * We use it for XML parsing, to disambiguate the use of '<' we need to
 * look at the token before.
 *)
let _last_non_whitespace_like_token = 
  ref (None: Parser_opa.token option)

let reset () = 
  _mode_stack := [default_state];
  _last_non_whitespace_like_token := None;
  ()

let push_mode mode = Common.push2 mode _mode_stack
let pop_mode () = ignore(Common.pop2 _mode_stack)

let rec current_mode () = 
  try 
    Common.top !_mode_stack
  with Failure("hd") -> 
    error("LEXER: mode_stack is empty, defaulting to INITIAL");
    reset();
    current_mode ()

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']
let operator = ['+' '\\' '-' '^' '*' '/' '<' '>' '=' '@' '|' '&' '!']

let newline = '\n'
let space = [' ' '\t']

let ident = (letter | '_') (letter | digit | '_')*

let xmltag = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''-']*
let xmlattr = xmltag


let nonzerodigit = ['1'-'9']
let bindigit = ['0'-'1']
let octdigit = ['0'-'7']
let hexdigit = digit | ['a'-'f'] | ['A'-'F']

(* note: most was was copied from python *)
let decimalinteger = nonzerodigit digit* | '0'
let octinteger = '0' 'o' octdigit+
let hexinteger = '0' ('x' | 'X') hexdigit+
let bininteger = '0' 'b' bindigit+

(*****************************************************************************)
(* Main Rule *)
(*****************************************************************************)

rule initial = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  | "//" [^ '\n']* { TComment (tokinfo lexbuf) }

  (* less: return a different token for /** comments ? TCommentDoc? *)
  | "/*" | "/**"
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
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }
  (* there was also "{{" "}}" in the classic syntax *)
  | "{" { 
      push_mode ST_INITIAL; 
      TOBrace(tokinfo lexbuf) 
    }  
  | "}" { 
      pop_mode (); 
      TCBrace(tokinfo lexbuf) 
    }

  | "." { TDot(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }

  | "->" { TArrow(tokinfo lexbuf) }
  | '_'  { TUnderscore(tokinfo lexbuf) }

  | '\\'  { TAntiSlash(tokinfo lexbuf) } (* classic syntax *)

  (* operators *)
  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) }  | "/" { TDiv(tokinfo lexbuf) }

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

  | "~" { TTilde(tokinfo lexbuf) }


  (* We need to disambiguate the different use of '<' to know whether 
   * we are in a position where an XML construct can be started. Knowing
   * what was the previous token seems enough; no need to hack the
   * grammar to have a global shared by the lexer and parser.
   * 
   * We could maybe even return a TSMALLER in both cases and still
   * not generate any conflict in the grammar, but it feels cleaner to 
   * generate a different token, because we will really change the lexing
   * mode when we will see a '>' which makes the parser enter in the
   * ST_IN_XML_TEXT state where it's ok to write "I don't like you"
   * in which the quote does not need to be ended.
   *)
   | "<" (xmltag as tag) { 
       match !_last_non_whitespace_like_token with
       | Some (
             TOParen _ | TCParen _
           | TOBrace _ | TCBrace _
           | TOBracket _ | TCBracket _
           | TEq _ | TComma _ | TColon _ 
           | Tif _ | Tthen _ | Telse _
           | TArrow _
           | TQuestion _
           (* can have a serie of xml at toplevel *)
           | T_XML_CLOSE_TAG _ | T_XML_SLASH_GT _
         )
         ->
           push_mode (ST_IN_XML_TAG tag);
           T_XML_OPEN_TAG(tag, tokinfo lexbuf)
       | _ -> 
           yyless (String.length tag) lexbuf;
           TLess(tokinfo lexbuf)
     }
   | "<>" {
       (* can't just put ("<" xmltag?) because then the previous rule
        * on the < symbol would take over
        *)
       yyless 1 lexbuf;
       let tag = "" in
       push_mode (ST_IN_XML_TAG tag);
       T_XML_OPEN_TAG(tag, tokinfo lexbuf)
     }

  (* Can define operators in OPA. This rule must be after other operators
   * lexing ruke (lex pick the first longest). Also need to define /**
   * above otherwise it would be parsed as an operator.
   *)
  | operator+ { TOp(tok lexbuf, tokinfo lexbuf) }


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
  (* todo? 'a, 'b'  type variables? ~label ? *)

  (* this was not mentioned in reference manual *)
  | "%%" ((letter | '_' | ['.'] | digit)+ as s) "%%" {
      TExternalIdent (s, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | (decimalinteger | octinteger | hexinteger | bininteger)
    { TInt (tok lexbuf, tokinfo lexbuf) }

  | digit (digit)* ('.' (digit)*)? ( ('e' |'E') ['+' '-']? digit (digit)* )?
     { TFloat (tok lexbuf, tokinfo lexbuf) }
  | '.' (digit)+ ( ('e' |'E') ['+' '-']? digit (digit)* )?
     { TFloat (tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  (* opa allows string interpolation => need a state in the lexer *)
  | '"' { 
      push_mode ST_DOUBLE_QUOTES;
      TGUIL(tokinfo lexbuf)
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

(*****************************************************************************)
(* String Rule *)
(*****************************************************************************)
and string_double_quote = parse
  | '"' { 
      pop_mode ();
      TGUIL(tokinfo lexbuf)
    }
  | '{' {
      push_mode ST_INITIAL;
      TOBrace(tokinfo lexbuf)
    }

  | '\\' ['\\' 'n' 'r' 't' '{' '}'  '\'' '"']
      { T_ENCAPSED(tok lexbuf, tokinfo lexbuf) }

  (* noteopti: must be the "negative" of the previous rules *)
  | [^ '{' '\\' '\"' '\n']* 
      { T_ENCAPSED(tok lexbuf, tokinfo lexbuf) }

  | eof { 
      error "LEXER: end of file in string_double_quote";
      TGUIL(tokinfo lexbuf)
    }
  | _  { let s = tok lexbuf in
         error ("LEXER: unrecognised symbol in string_double_quote:"^s);
         T_ENCAPSED(s, tokinfo lexbuf)
    }

(*****************************************************************************)
(* Html Rule *)
(*****************************************************************************)
and in_xml_tag current_tag = parse
  (* todo? allow comments too there ? *)
  | [' ' '\t']+ { TCommentSpace(tokinfo lexbuf) }
  | ['\n' '\r'] { TCommentNewline(tokinfo lexbuf) }

  (* attribute management *)
  | xmlattr { T_XML_ATTR(tok lexbuf, tokinfo lexbuf) }
  | "="     { TEq(tokinfo lexbuf) }
  | '"' {
      push_mode ST_DOUBLE_QUOTES;
      TGUIL(tokinfo lexbuf)
    }
  | "{" {
      push_mode ST_INITIAL; 
      TOBrace(tokinfo lexbuf)
    }
  (* todo: concat them? *)
  | "#" (ident as s) { TSharpIdent(s, tokinfo lexbuf) }

  (* When we see a ">", it means it's just the end of 
   * the opening tag. Transit to IN_XML_TEXT.
   *)
  | ">" { 
      pop_mode();
      push_mode (ST_IN_XML_TEXT current_tag);
      T_XML_MORE (tokinfo lexbuf)
    }

  (* a singleton tag. Was not mentionned in reference manual I think *)
  | "/>" { 
      pop_mode ();
      T_XML_SLASH_GT (tokinfo lexbuf) 
    }

  | eof { EOF (tokinfo lexbuf +> Parse_info.rewrap_str "") }
  | _  { let s = tok lexbuf in
         error ("LEXER: unrecognised symbol in in_xml_tag:"^s);
         TUnknown(tokinfo lexbuf)
   }

and in_xml_text current_tag = parse

  (* a nested xml construct *)
  | "<" (xmltag as tag) { 
      push_mode (ST_IN_XML_TAG tag);
      T_XML_OPEN_TAG(tag, tokinfo lexbuf)
    }

  | "<" "/" (xmltag as tag) ">" { 
      if (tag <> current_tag)
      then error (spf "XML: wrong closing tag for, %s != %s" tag current_tag);
      pop_mode ();
      T_XML_CLOSE_TAG(Some tag, tokinfo lexbuf)
    }

  | "</>" { 
      (* no check :( *)
      pop_mode ();
      T_XML_CLOSE_TAG(None, tokinfo lexbuf)
    }

  (* Interpolation *)
  | "{" {
      push_mode ST_INITIAL; 
      TOBrace(tokinfo lexbuf)
    }

  (* opti: *)
  | [^'<' '{']+ { T_XML_TEXT (tok lexbuf, tokinfo lexbuf) }
  | _  { let s = tok lexbuf in
         error ("LEXER: unrecognised symbol in in_xml_text:"^s);
         TUnknown(tokinfo lexbuf)
   }
