{
type error =
  | Illegal_character of char
  | Bad_entity of string
  | Unterminated of string
  | Tag_expected
  | Attribute_expected
  | Other of string

let error_string = function
  | Illegal_character c ->
      "illegal character '" ^ Char.escaped c ^ "'"
  | Bad_entity s ->
      "\"&" ^ s ^ ";\" is not a valid entity"
  | Unterminated s -> "unterminated " ^ s ^ " starts here"
  | Tag_expected -> "a tag was expected"
  | Attribute_expected -> "an attribute value was expected"
  | Other s -> s

exception Error of error * int

type token =
  | Tag of string * (string * string) list * bool
  | Chars of string
  | Endtag of string
  | EOF

let start_pos = ref 0
let reset_pos lexbuf =
  start_pos := Lexing.lexeme_start lexbuf

let raise_unterminated msg =
  raise (Error (Unterminated msg, !start_pos))

let buffer = Buffer.create 128
let reset_string lexbuf =
  reset_pos lexbuf ;
  Buffer.reset buffer

let strip_ws = ref true
let entities = ref [ "lt"  , "<"; 
		     "gt"  , ">"; 
		     "amp" , "&"; 
		     "apos", "'"; 
		     "quot", "\"" ]

let ws = [ ' '; '\009'; '\010'; '\012'; '\013' ]
let trim_ws s =
  let len = String.length s in
  let start = ref 0 in
  let stop = ref (len - 1) in
  while !start < len && List.mem s.[!start] ws do
    incr start 
  done ;
  while !stop >= !start && List.mem s.[!stop] ws do
    decr stop
  done ;
  if !start <> 0 || !stop <> len - 1
  then String.sub s !start (!stop - !start + 1)
  else s
}

let space = [' ' '\009' '\010' '\012' '\013']
let name  = ['A'-'Z' 'a'-'z' '_' ':'] ['A'-'Z' 'a'-'z' '0'-'9' '_' ':' '.' '-']*

rule token = parse
  | "<!--"
      { reset_pos lexbuf; comment lexbuf; token lexbuf }
  | "<!DOCTYPE"
      { reset_pos lexbuf; skip_doctype lexbuf; token lexbuf }
  | "<![CDATA["
      { reset_string lexbuf ;
	match cdata lexbuf with
	| "" -> token lexbuf
	| s  -> Chars (cdata lexbuf) }
  | "<?"
      { reset_pos lexbuf; skip_prolog_or_pi lexbuf; token lexbuf }
  | "</"
      { let tag = tag_name lexbuf in 
        close_tag lexbuf ;
        Endtag tag }
  | '<'
      { let tag = tag_name lexbuf in
        let attribs, closed = attributes lexbuf in
        Tag (tag, attribs, closed) }
  | eof
      { EOF }
  | ""
      { reset_string lexbuf;
	match chars lexbuf with
	| "" -> token lexbuf
	| s  -> Chars s }

and chars = parse
  | [ ^ '&' '<' ] +
      { let len = lexbuf.Lexing.lex_curr_pos - lexbuf.Lexing.lex_start_pos in
	Buffer.add_substring buffer lexbuf.Lexing.lex_buffer lexbuf.Lexing.lex_start_pos len ;
        chars lexbuf }
  | '&'
      { Buffer.add_string buffer (entity lexbuf) ;
	chars lexbuf }
  | "" | eof
      { if !strip_ws
        then trim_ws (Buffer.contents buffer) 
        else Buffer.contents buffer }

and entity = parse
  | '#'
      { let s = Gutf8.from_unichar (charref lexbuf) in
        String.iter (fun c -> Printf.eprintf "%x" (Char.code c)) s;
        prerr_endline ""; s }
  | name ';'
      { let l = Lexing.lexeme lexbuf in
        let s = String.sub l 0 (String.length l - 1) in
        try List.assoc s !entities
        with Not_found -> raise (Error(Bad_entity s, Lexing.lexeme_start lexbuf)) }
  | _
      { raise_unterminated "entity" }

and charref = parse
  | 'x' ['0'-'9' 'a'-'f' 'A'-'F']+ ';'
      { let l = Lexing.lexeme lexbuf in
        let v = String.sub l 1 (String.length l - 2) in
	int_of_string ("0x" ^ v) }
  | ['0'-'9' ]+ ';'
      { let l = Lexing.lexeme lexbuf in
        let v = String.sub l 0 (String.length l - 1) in
	int_of_string v }
  | _
      { raise (Error(Other "bad character reference", Lexing.lexeme_start lexbuf)) }

and tag_name = parse
  | name
      { Lexing.lexeme lexbuf }
  | _
      { raise (Error(Tag_expected, Lexing.lexeme_start lexbuf)) }

and close_tag = parse
  | space* '>'
      { () }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Lexing.lexeme_start lexbuf)) }

and attributes = parse
  | space+
      { attributes lexbuf }
  | '>'
      { [], false }
  | "/>"
      { [], true }
  | ""
      { let key = tag_name lexbuf in
        eq lexbuf ;
        let data = attribute_data lexbuf in
        let others, closed = attributes lexbuf in
        (key, data) :: others, closed }

and eq = parse
  | space* '=' space*
      { () }
  | _ 
      { raise (Error(Attribute_expected, Lexing.lexeme_start lexbuf)) }

and attribute_data = parse
  | '"'
      { reset_string lexbuf; dq_string lexbuf }
  | '''
      { reset_string lexbuf;  q_string lexbuf }
  | _
      { raise (Error(Attribute_expected, Lexing.lexeme_start lexbuf)) }

and cdata = parse
  | "]]>"
      { Buffer.contents buffer }
  | eof
      { raise_unterminated "CDATA" }
  | _
      { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0) ;
	cdata lexbuf }

and q_string = parse
  | '''
      { Buffer.contents buffer }
  | '&'
      { Buffer.add_string buffer (entity lexbuf); 
	q_string lexbuf }
  | eof
      { raise_unterminated "string" }
  | _
      { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0); 
	q_string lexbuf }

and dq_string = parse
  | '"'
      { Buffer.contents buffer }
  | '&'
      { Buffer.add_string buffer (entity lexbuf); 
	dq_string lexbuf }
  | eof
      { raise_unterminated "string" }
  | _
      { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0); 
	dq_string lexbuf }

and comment = parse
  | "-->"
      { () }
  | eof
      { raise_unterminated "comment" }
  | _
      { comment lexbuf }

and skip_prolog_or_pi = parse
  | "?>"
      { () }
  | eof
      { raise_unterminated "prolog or PI" }
  | _
      { skip_prolog_or_pi lexbuf }

and skip_doctype = parse
  | '"' [^ '"' ]* '"'
      { skip_doctype lexbuf }
  | ''' [^ ''' ]* '''
      { skip_doctype lexbuf }
  | '['
      { skip_intsubset lexbuf; skip_doctype lexbuf }
  | '>'
      { () }
  | eof
      { raise_unterminated "DOCTYPE" }
  | [^ ''' '"' '[' '>']+
      { skip_doctype lexbuf }

and skip_intsubset = parse
  | ']' | eof
      { () }
  | '"' [^ '"' ]* '"'
      { skip_intsubset lexbuf }
  | ''' [^ ''' ]* '''
      { skip_intsubset lexbuf }
  | [^ ''' '"' ']']+
      { skip_intsubset lexbuf }


