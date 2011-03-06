{
(*
 * Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
 * Copyright (C) 2011 Facebook
 * 
 * This software is distributed under the terms of the GNU GPL version 2.
 * See LICENSE file for full license text.
 *
 *)

open Common 

open Parse_css

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Most of the code in this file is copy pasted from Dario Teixera
 * css parser and preprocessor: http://forge.ocamlcore.org/projects/ccss/.
 * I've mostly converted it from ulex to ocamllex.
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

(*
let add_lines nlines lexbuf =
	let adder acc el = if el = 10 then acc+1 else acc in
	let lexeme = Ulexing.lexeme lexbuf
	in nlines + (Array.fold_left adder 0 lexeme)


let trim_lexbuf ?(left = 0) ?(right = 0) lexbuf =
	Ulexing.utf8_sub_lexeme lexbuf left ((Ulexing.lexeme_length lexbuf) - left - right)


let ltrim_lexbuf lexbuf =
	trim_lexbuf ~left:1 lexbuf


let rtrim_lexbuf lexbuf =
	trim_lexbuf ~right:1 lexbuf


let parse_quantity =
	let rex = Pcre.regexp "(?<number>(\\+|-)?[0-9]+(\\.[0-9]+)?)(?<units>%|[A-Za-z]+)?"
	in fun lexbuf ->
		let subs = Pcre.exec ~rex (Ulexing.utf8_lexeme lexbuf) in
		let number = Pcre.get_named_substring rex "number" subs
		and units = try Some (Pcre.get_named_substring rex "units" subs) with Not_found -> None
		in (float_of_string number, units)


*)
}

(*****************************************************************************)

let alpha = ['a'-'z']
let digit = ['0'-'9']
let hexa = ['0'-'9' 'a'-'f']
let space = [' ' '\t' '\n']

let ident = ['a'-'z' '-'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']*
let variable = ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']*
let hashed = '#' ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']+

let number = ('-' | '+')? digit+ ('.' digit+)?
let units = alpha+ | '%'
let slc = "//" [^ '\n']+
let nth = ('-' | '+')? digit+ 'n' ('-' | '+') digit+

(*****************************************************************************)

let rec main_scanner = parse
  | "url("     { URI }
  | ident '('  { TERM_FUNC (rtrim_lexbuf lexbuf) }
  | ':' ident '(' { SEL_FUNC (trim_lexbuf ~right:1 ~left:1 lexbuf) }
  | nth	          { NTH (Ulexing.utf8_lexeme lexbuf) }
  | number units? { QUANTITY (parse_quantity lexbuf) }
  | ident	{ IDENT (Ulexing.utf8_lexeme lexbuf) }
  | variable	{ VAR (Ulexing.utf8_lexeme lexbuf) }
  | hashed	{ HASH (ltrim_lexbuf lexbuf) }
  | "@charset" space+	{ CHARSET }
  | "@import" space+	{ IMPORT }
  | "@media" space+	{ MEDIA }
  | "@page" space+	{ PAGE }
  | "@font-face" space+	{ FONTFACE }
  | space* "!important" space*	{ IMPORTANT }
  | space* "/*"	{comment_scanner lexbuf }
  | space* slc space*	{ main_scanner (add_lines nlines lexbuf) lexbuf }
  | "="		{ ATTR_EQUALS }
  | "~="	{ ATTR_INCLUDES }
  | "|="	{ ATTR_DASHMATCH }
  | "^="	{ ATTR_PREFIX }
  | "$="	{ ATTR_SUFFIX }
  | "*="	{ ATTR_SUBSTRING }

  | space* "::" space*	{ DOUBLE_COLON }
  | space* '*' space*	{ ASTERISK }
  (* 247 is the decimal Unicode codepoint for the division sign *)
  | space* 247 space*	{ QUOTIENT }	
  | space* '/' space*	{ SLASH }
  | space* '+' space*	{ PLUS }
  | space+ '-' space+	{ MINUS }
  | space* '~' space*	{ TILDE }
  | space* '>' space*	{ GT }
  | space* '{' space*	{ OPEN_CURLY }
  | space* '}' space*	{ CLOSE_CURLY }
  | space* ';' space*	{ SEMICOLON }
  | space* ':' space*	{ COLON }
  | space* ',' space*	{ COMMA }
  | space* '(' space*	{ OPEN_ROUND }
  | space* ')' space*	{ CLOSE_ROUND }
  | '.'			{ PERIOD }
  | '['			{ OPEN_SQUARE }
  | ']'			{ CLOSE_SQUARE }

  | '\''		-> single_string_scanner nlines "" lexbuf
  | '"'			-> double_string_scanner nlines "" lexbuf
  | space		-> (add_lines nlines lexbuf, S)
  | eof			-> (nlines, EOF)
  | _			-> raise (Error (Ulexing.utf8_lexeme lexbuf))
            
and single_string_scanner nlines accum = parser
  | '\''  -> (nlines, STRING accum)
  | _	  -> single_string_scanner (add_lines nlines lexbuf) (accum ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


and double_string_scanner nlines accum = lexer
  | '"'		-> (nlines, STRING accum)
  | _		-> double_string_scanner (add_lines nlines lexbuf) (accum ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


and comment_scanner nlines = lexer
  | "*/" space*			-> main_scanner (add_lines nlines lexbuf) lexbuf
  | _				-> comment_scanner (add_lines nlines lexbuf) lexbuf

