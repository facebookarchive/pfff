{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013, 2014 Facebook
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

open Parser_js
module Flag = Flag_parsing_js
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Javascript lexer.
 *
 * src: ocamllexified from Marcel Laverdet 'fbjs2'?
 *
 * There are a few tricks to go around ocamllex restrictions
 * because Javascript now have different lexing rules depending on some
 * "contexts", especially for JSX/XHP (this is similar to Perl,
 * e.g. the <<<END context).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
exception Lexical of string

let tok     lexbuf  =
  Lexing.lexeme lexbuf
let tokinfo lexbuf  =
  PI.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

let error s =
  if !Flag.exn_when_lexical_error
  then raise (Lexical (s))
  else
    if !Flag.verbose_lexing
    then pr2_once ("LEXER: " ^ s)
    else ()

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

(* less: should use Buffer and not ^ so we should not need that *)
let tok_add_s s ii  =
  PI.rewrap_str ((PI.str_of_info ii) ^ s) ii

let hexa_to_int = function
  | '0'..'9' as x -> Char.code x - Char.code '0'
  | 'a'..'f' as x -> Char.code x - Char.code 'a' + 10
  | 'A'..'F' as x -> Char.code x - Char.code 'A' + 10
  | _ -> assert false;;

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

  "interface",      (fun ii -> T_INTERFACE ii);
]

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
(* note: mostly a copy paste of the trick used in the lexer for PHP *)

type state_mode =
  (* Regular Javascript mode *)
  | ST_IN_CODE
  (* started with <xx when preceded by a certain token (e.g. 'return' '<xx'),
   * finished by '>' by transiting to ST_IN_XHP_TEXT, or really finished
   * by '/>'.
   *)
  | ST_IN_XHP_TAG of string (* the current tag, e,g, "x_frag" *)
  (* started with the '>' of an opening tag, finished when '</x>' *)
  | ST_IN_XHP_TEXT of string (* the current tag *)
  (* started with "`", finished with "`" *)
  | ST_IN_BACKQUOTE

let default_state = ST_IN_CODE

let _mode_stack =
  ref [default_state]

(* The logic to modify _last_non_whitespace_like_token is in the
 * caller of the lexer, that is in Parse_js.tokens.
 * Used for ambiguity between / as a divisor and start of regexp.
 *)
let _last_non_whitespace_like_token =
  ref (None: Parser_js.token option)

let reset () =
  _mode_stack := [default_state];
   _last_non_whitespace_like_token := None;
  ()

let rec current_mode () =
  try
    Common2.top !_mode_stack
  with Failure("hd") ->
    error("mode_stack is empty, defaulting to INITIAL");
    reset();
    current_mode ()

let push_mode mode = Common.push mode _mode_stack
let pop_mode () = ignore(Common2.pop2 _mode_stack)
let set_mode mode =
  pop_mode();
  push_mode mode;
  ()

(* Here is an example of state transition. Given a js file like:
 *
 *   return <x>foo<y>bar</y></x>;
 *
 * we start with the stack in [ST_IN_CODE]. The transitions are then:
 *
 * 'return' -> [IN_CODE]
 * '<x'     -> [IN_XHP_TAG "x"; IN_CODE], via push_mode()
 * '>'      -> [IN_XHP_TEXT "x"; IN_CODE], via set_mode()
 * 'foo'    -> [IN_XHP_TEXT "x"; IN_CODE]
 * '<y'     -> [IN_XHP_TAG "y";IN_XHP_TEXT "x"; IN_CODE], via push_mode()
 * '>'      -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x";IN_CODE], via set_mode()
 * 'bar'    -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x"; IN_CODE]
 * '</y>'   -> [IN_XHP_TEXT "x"; IN_CODE], via pop_mode()
 * '</x>'   -> [IN_CODE], via pop_mode()
 * ';'      -> [IN_CODE]
 *
 *)

}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let NEWLINE = ("\r"|"\n"|"\r\n")
let HEXA = ['0'-'9''a'-'f''A'-'F']

let XHPLABEL =	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''-']*
let XHPTAG = XHPLABEL (":" XHPLABEL)*
let XHPATTR = XHPLABEL (":" XHPLABEL)*

let InputCharacter = [^ '\r' '\n']

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule initial = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      Buffer.add_string buf "/*";
      st_comment buf lexbuf;
      TComment(info +> PI.rewrap_str (Buffer.contents buf))
    }

  (* don't keep the trailing \n; it will be in another token *)
  | "//" InputCharacter* { TComment (tokinfo lexbuf) }
  (* should be accepted only at the beginning of the file *)
  | "#!" InputCharacter* { TComment (tokinfo lexbuf) }
  | [' ' '\t']+  { TCommentSpace(tokinfo lexbuf) }
  | NEWLINE      { TCommentNewline(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "{" {
    push_mode ST_IN_CODE;
    T_LCURLY (tokinfo lexbuf);
  }
  | "}" {
    pop_mode ();
    T_RCURLY (tokinfo lexbuf);
  }

  | "(" { T_LPAREN (tokinfo lexbuf) }
  | ")" { T_RPAREN (tokinfo lexbuf) }

  | "[" { T_LBRACKET (tokinfo lexbuf) }
  | "]" { T_RBRACKET (tokinfo lexbuf) }
  | "." { T_PERIOD (tokinfo lexbuf) }
  | ";" { T_SEMICOLON (tokinfo lexbuf) }
  | "," { T_COMMA (tokinfo lexbuf) }
  | ":" { T_COLON (tokinfo lexbuf) }
  | "?" { T_PLING (tokinfo lexbuf) }
  | "&&" { T_AND (tokinfo lexbuf) }
  | "||" { T_OR (tokinfo lexbuf) }
  | "===" { T_STRICT_EQUAL (tokinfo lexbuf) }
  | "!==" { T_STRICT_NOT_EQUAL (tokinfo lexbuf) }
  | "<=" { T_LESS_THAN_EQUAL (tokinfo lexbuf) }
  | ">=" { T_GREATER_THAN_EQUAL (tokinfo lexbuf) }
  | "==" { T_EQUAL (tokinfo lexbuf) }
  | "!=" { T_NOT_EQUAL (tokinfo lexbuf) }
  | "++" { T_INCR (tokinfo lexbuf) }
  | "--" { T_DECR (tokinfo lexbuf) }
  | "<<=" { T_LSHIFT_ASSIGN (tokinfo lexbuf) }
  | "<<" { T_LSHIFT (tokinfo lexbuf) }
  | ">>=" { T_RSHIFT_ASSIGN (tokinfo lexbuf) }
  | ">>>=" { T_RSHIFT3_ASSIGN (tokinfo lexbuf) }
  | ">>>" { T_RSHIFT3 (tokinfo lexbuf) }
  | ">>" { T_RSHIFT (tokinfo lexbuf) }
  | "+=" { T_PLUS_ASSIGN (tokinfo lexbuf) }
  | "-=" { T_MINUS_ASSIGN (tokinfo lexbuf) }

  | "*=" { T_MULT_ASSIGN (tokinfo lexbuf) }
  | "%=" { T_MOD_ASSIGN (tokinfo lexbuf) }
  | "&=" { T_BIT_AND_ASSIGN (tokinfo lexbuf) }
  | "|=" { T_BIT_OR_ASSIGN (tokinfo lexbuf) }
  | "^=" { T_BIT_XOR_ASSIGN (tokinfo lexbuf) }
  (* see also xhp code for handling "< XHPTAG" below *)
  | "<" { T_LESS_THAN (tokinfo lexbuf) }
  | ">" { T_GREATER_THAN (tokinfo lexbuf) }
  | "+" { T_PLUS (tokinfo lexbuf) }
  | "-" { T_MINUS (tokinfo lexbuf) }
  | "*" { T_MULT (tokinfo lexbuf) }
  (* for '/' see below the regexp handling *)
  | "%" { T_MOD (tokinfo lexbuf) }
  | "|" { T_BIT_OR (tokinfo lexbuf) }
  | "&" { T_BIT_AND (tokinfo lexbuf) }
  | "^" { T_BIT_XOR (tokinfo lexbuf) }
  | "!" { T_NOT (tokinfo lexbuf) }
  | "~" { T_BIT_NOT (tokinfo lexbuf) }
  | "=" { T_ASSIGN (tokinfo lexbuf) }

  (* arrows (aka short lambdas) *)
  | "=>" { T_ARROW (tokinfo lexbuf) }
  (* variable number of parameters, less: enforce directly attached to ident? *)
  | "..." { T_DOTS (tokinfo lexbuf) }

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

  | "0" ['X''x'] HEXA+ {
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
  | ("'"|'"') as quote {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      let s = Buffer.contents buf in
      let buf2 = Buffer.create 127 in
      Buffer.add_char buf2 quote;
      Buffer.add_string buf2 s;
      Buffer.add_char buf2 quote;
      (* s does not contain the enclosing "'" but the info does *)
      T_STRING (s, info +> PI.rewrap_str (Buffer.contents buf2))
    }

  (* ----------------------------------------------------------------------- *)
  (* Backquote (interpolated) strings  *)
  (* ----------------------------------------------------------------------- *)

  | "`" {
      push_mode ST_IN_BACKQUOTE;
      T_BACKQUOTE(tokinfo lexbuf)
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
   *)
  | "/" | "/=" {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in

      match !_last_non_whitespace_like_token with
      | Some (
            T_IDENTIFIER _
          | T_NUMBER _ | T_STRING _ | T_REGEX _
          | T_FALSE _ | T_TRUE _ | T_NULL _
          | T_THIS _
          | T_INCR _ | T_DECR _
          | T_RBRACKET _ | T_RPAREN _
        ) ->
          (match s with
          | "/" -> T_DIV (info)
          | "/=" -> T_DIV_ASSIGN (tokinfo lexbuf)
          | _ -> raise Impossible
          )
      | _ ->
          let buf = Buffer.create 127 in
          Buffer.add_string buf s;
          regexp buf lexbuf;
          let str = Buffer.contents buf in
          T_REGEX (str, info +> PI.rewrap_str str)
    }

  (* ----------------------------------------------------------------------- *)
  (* XHP *)
  (* ----------------------------------------------------------------------- *)

  (* xhp: we need to disambiguate the different use of '<' to know whether
   * we are in a position where an XHP construct can be started. Knowing
   * what was the previous token seems enough; no need to hack the
   * grammar to have a global shared by the lexer and parser.
   *
   * We could maybe even return a TSMALLER in both cases and still
   * not generate any conflict in the grammar, but it feels cleaner to
   * generate a different token, because we will really change the lexing
   * mode when we will see a '>' which makes the parser enter in the
   * ST_IN_XHP_TEXT state where it's ok to write "I don't like you"
   * in which the quote does not need to be ended.
   *)
  | "<" (XHPTAG as tag) {

    match !_last_non_whitespace_like_token with
    | Some (
        T_LPAREN _
      | T_SEMICOLON _ | T_COMMA _
      | T_LCURLY _ | T_RCURLY _
      | T_RETURN _
      | T_ASSIGN _
      | T_ARROW _
      | T_PLING _ | T_COLON _
      | T_LBRACKET _
      | T_AND _ | T_OR _ | T_PLUS _
    )
      ->
      push_mode (ST_IN_XHP_TAG tag);
      T_XHP_OPEN_TAG(tag, tokinfo lexbuf)
    | _ ->
      yyback (String.length tag) lexbuf;
      T_LESS_THAN(tokinfo lexbuf)
  }

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

and string_escape quote buf = parse
  | '\\'{ Buffer.add_string buf "\\\\" }
  | 'x' (HEXA as a) (HEXA as b)
    { let code = hexa_to_int a * 16 + hexa_to_int b in
      if code > 127
      then
        let c1 = code lsr 6 + 0xC0
        and c2 = code land 0x3f + 0x80 in
        Buffer.add_char buf (Char.chr c1);
        Buffer.add_char buf (Char.chr c2)
      else Buffer.add_char buf (Char.chr code) }
  | 'u' HEXA HEXA HEXA HEXA {
      Buffer.add_char buf '\\';
      Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | (_ as c)
    { Buffer.add_char buf '\\'; Buffer.add_char buf c }
      (* if c = quote *)
      (* then Buffer.add_char buf quote *)
      (* else Buffer.add_char buf c } *)


and string_quote q buf = parse
  | ("'"|'"') as q' {
    if q = q'
    then ()
    else (Buffer.add_char buf q'; string_quote q buf lexbuf) }
  | '\\' {
      string_escape q buf lexbuf;
      string_quote q buf lexbuf
    }
  | (_ as x) { Buffer.add_char buf x; string_quote q buf lexbuf }
  | eof      { error "WIERD end of file in quoted string" }


and backquote = parse
  | "`" {
    pop_mode();
    T_BACKQUOTE(tokinfo lexbuf)
  }
  | "${" {
    push_mode ST_IN_CODE;
    T_DOLLARCURLY(tokinfo lexbuf)
  }
  (* todo: what about escape \\ chars? can escape `? *)
  | [^'`' '$']+ {
      T_ENCAPSED_STRING(tok lexbuf, tokinfo lexbuf)
    }
  | "$" {
      T_ENCAPSED_STRING(tok lexbuf, tokinfo lexbuf)
    }

  | eof { EOF (tokinfo lexbuf +> PI.rewrap_str "") }
  | _  {
      error ("unrecognised symbol, in backquote string:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }


(*****************************************************************************)
(* Rule regexp *)
(*****************************************************************************)
and regexp buf = parse
  | '/' { Buffer.add_char buf '/'; regexp_maybe_ident buf lexbuf }
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    (* check char ? *)
                    Buffer.add_char buf x;
                    regexp buf lexbuf }
  | '[' { Buffer.add_char buf '['; regexp_class buf lexbuf }
  | (_ as x)       { Buffer.add_char buf x; regexp buf lexbuf }
  | eof { error "WIERD end of file in regexp"; ()}


and regexp_class buf = parse
  | ']' { Buffer.add_char buf ']';
             regexp buf lexbuf }
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    Buffer.add_char buf x;
                    regexp_class buf lexbuf }
  | (_ as x) { Buffer.add_char buf x; regexp_class buf lexbuf }

and regexp_maybe_ident buf = parse
  | ['A'-'Z''a'-'z']* { Buffer.add_string buf (tok lexbuf) }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)

and st_comment buf = parse
  | "*/"    { Buffer.add_string buf (tok lexbuf) }
  (* noteopti: *)
  | [^'*']+ { Buffer.add_string buf (tok lexbuf); st_comment buf lexbuf }
  | "*"     { Buffer.add_string buf (tok lexbuf); st_comment buf lexbuf }
  | eof     { error "end of file in comment" }
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s);
      Buffer.add_string buf s;
      st_comment buf lexbuf
    }

(*****************************************************************************)
(* Rules for XHP *)
(*****************************************************************************)
(* XHP lexing states and rules *)

and st_in_xhp_tag current_tag = parse

  (* The original XHP parser have some special handlings of
   * whitespace and enforce to use certain whitespace at
   * certain places. Not sure I need to enforce this too.
   * Simpler to ignore whitespaces.
   *
   * todo? factorize with st_in_scripting rule?
   *)
  | [' ' '\t']+ { TCommentSpace(tokinfo lexbuf) }
  | ['\n' '\r'] { TCommentNewline(tokinfo lexbuf) }
  | "/*" {
        let info = tokinfo lexbuf in
        let buf = Buffer.create 127 in
        Buffer.add_string buf "/*";
        st_comment buf lexbuf;
        TComment(info +> PI.rewrap_str (Buffer.contents buf))
     }
  | "/**/" { TComment(tokinfo lexbuf) }

  | "//" InputCharacter* { TComment(tokinfo lexbuf) }

  (* attribute management *)
  | XHPATTR { T_XHP_ATTR(tok lexbuf, tokinfo lexbuf) }
  | "="     { T_ASSIGN(tokinfo lexbuf) }

  | ("'"|'"') as quote {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      let s = Buffer.contents buf in
      let buf2 = Buffer.create 127 in
      Buffer.add_char buf2 quote;
      Buffer.add_string buf2 s;
      Buffer.add_char buf2 quote;
      (* s does not contain the enclosing "'" but the info does *)
      T_STRING (s, info +> PI.rewrap_str (Buffer.contents buf2))
    }

  | "{" {
      push_mode ST_IN_CODE;
      T_LCURLY(tokinfo lexbuf)
    }

  (* a singleton tag *)
  | "/>" {
      pop_mode ();
      T_XHP_SLASH_GT (tokinfo lexbuf)
    }

  (* When we see a ">", it means it's just the end of
   * the opening tag. Transit to IN_XHP_TEXT.
   *)
  | ">" {
      set_mode (ST_IN_XHP_TEXT current_tag);
      T_XHP_GT (tokinfo lexbuf)
    }

  | eof { EOF (tokinfo lexbuf +> PI.rewrap_str "") }
  | _  {
        error("unrecognised symbol, in XHP tag:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
    }

(* ----------------------------------------------------------------------- *)
and st_in_xhp_text current_tag = parse

  (* a nested xhp construct *)
  | "<" (XHPTAG as tag) {

      push_mode (ST_IN_XHP_TAG tag);
      T_XHP_OPEN_TAG(tag, tokinfo lexbuf)
    }

  | "<" "/" (XHPTAG as tag) ">" {
      if (tag <> current_tag)
      then error (spf "XHP: wrong closing tag for, %s != %s" tag current_tag);
      pop_mode ();
      T_XHP_CLOSE_TAG(Some tag, tokinfo lexbuf)

    }
  (* shortcut for closing tag ? *)
  | "<" "/" ">" {
      (* no check :( *)
      pop_mode ();
      T_XHP_CLOSE_TAG(None, tokinfo lexbuf)
    }
  | "<!--" {
      let info = tokinfo lexbuf in
      let com = st_xhp_comment lexbuf in
      (* less: make a special token T_XHP_COMMENT? *)
      TComment(info +> tok_add_s com)
  }

  (* PHP interpolation. How the user can produce a { ? &;something ? *)
  | "{" {
      push_mode ST_IN_CODE;
      T_LCURLY(tokinfo lexbuf)
    }

  (* opti: *)
  | [^'<' '{']+ { T_XHP_TEXT (tok lexbuf, tokinfo lexbuf) }

  | eof { EOF (tokinfo lexbuf +> PI.rewrap_str "") }
  | _  {
      error ("unrecognised symbol, in XHP text:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

and st_xhp_comment = parse
  | "-->" { tok lexbuf }
  | [^'-']+ { let s = tok lexbuf in s ^ st_xhp_comment lexbuf }
  | "-"     { let s = tok lexbuf in s ^ st_xhp_comment lexbuf }
  | eof { error "end of file in xhp comment"; "-->"}
  | _  {
      let s = tok lexbuf in
      error("unrecognised symbol in xhp comment:"^s);
      s ^ st_xhp_comment lexbuf
    }
