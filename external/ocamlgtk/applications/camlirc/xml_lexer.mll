(**************************************************************************)
(*     Lablgtk - Camlirc                                                  *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Nobuaki Yoshida  <nyoshi@dd.iij4u.or.jp>                          *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: xml_lexer.mll 1354 2007-07-20 04:18:38Z garrigue $ *)

{
open Lexing

type error =
  | Illegal_character of char
  | Bad_entity of string
  | Unterminated of string
  | Tag_expected
  | Other of string

let error_string = function
  | Illegal_character c ->
      "illegal character '" ^ Char.escaped c ^ "'"
  | Bad_entity s ->
      "\"&" ^ s ^ ";\" is not a valid entity"
  | Unterminated s -> "unterminated " ^ s ^ " starts here"
  | Tag_expected -> "a tag was expected"
  | Other s -> s

exception Error of error * int

type token =
  | Tag of string * (string * string) list * bool
  | Chars of string
  | Endtag of string
  | EOF

let string_start_pos = ref 0
and comment_start_pos = ref 0
and token_start_pos = ref 0

let token_start () = !token_start_pos

let string_buffer = Buffer.create 80
let reset_string lexbuf =
  string_start_pos := lexeme_start lexbuf;
  Buffer.reset string_buffer

let reset_comment lexbuf =
  comment_start_pos := lexeme_start lexbuf

let entities = [ "lt", "<"; "gt", ">"; "ampers", "&" ]

}

let break = ['\010' '\013' '\012']
let space = [' ' '\009']
let identchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9']

rule token = parse
  | break +
      { token lexbuf }
  | space +
      { token lexbuf }

  | "<!--"
      { reset_comment lexbuf; comment lexbuf; token lexbuf }
  | "</"
      { token_start_pos := lexeme_start lexbuf;
        let tag = tag_name lexbuf in close_tag lexbuf; Endtag tag }
        
  | "<"
      { token_start_pos := lexeme_start lexbuf;
        let tag = tag_name lexbuf in
        let attribs, closed = attributes lexbuf in
        Tag(tag, attribs, closed) }
  | space * [ ^ ' ' '\009' '\010' '\013' '\012' '<' '>' '&'] +
      { token_start_pos := lexeme_start lexbuf;
        reset_string lexbuf;
        Buffer.add_string string_buffer (lexeme lexbuf);
        Chars(chars lexbuf) }
  | "&"
      { token_start_pos := lexeme_start lexbuf;
        reset_string lexbuf;
        Buffer.add_string string_buffer (entity lexbuf);
        Chars(chars lexbuf)  }
  | eof
      { EOF }
  | _
      { raise (Error(Illegal_character (lexeme_char lexbuf 0),
                     lexeme_start lexbuf)) }

and chars = parse
  | [ ^ '\010' '\013' '\012' '<' '>' '&' ] +
      { Buffer.add_string string_buffer (lexeme lexbuf);
        chars lexbuf }
  | "&"
      { Buffer.add_string string_buffer (entity lexbuf);
        chars lexbuf  }
  | ""
      { Buffer.contents string_buffer }

and entity = parse
  | identchar + ";"
      { let s = lexeme lexbuf in
      let s = String.sub s 0 (String.length s - 1) in
        try List.assoc (String.lowercase s) entities
        with Not_found ->
          "&" ^ String.lowercase s ^ ";" }
  | _
      { raise (Error (Unterminated "entity", lexeme_start lexbuf)) }

and tag_name = parse
  | ('!' ?) (identchar +)
      { String.lowercase (lexeme lexbuf) }
  | _
      { raise (Error(Tag_expected, lexeme_start lexbuf)) }

and close_tag = parse
  | (space|break) +
      { close_tag lexbuf }
  | ">"
      { () }
  | _
      { raise (Error(Illegal_character (lexeme_char lexbuf 0),
                     lexeme_start lexbuf)) }

and attributes = parse
  | (space|break) +
      { attributes lexbuf }
  | ">"
      { [], false }
  | "/>"
      { [], true }
  | ""
      { let key = attribute lexbuf in
        let data = attribute_data lexbuf in
        let others, closed = attributes lexbuf in
        (String.lowercase key, data) :: others, closed }

and attribute = parse
  | '"'
      { reset_string lexbuf; string lexbuf }
  | [ ^ ' ' '\010' '\013' '\009' '\012' '=' '<' '>' '"' ] +
      { lexeme lexbuf }

and attribute_data = parse
  | "=" { attribute lexbuf }
  | ""  { "" }

and string = parse
  | '"'
      { Buffer.contents string_buffer }
  | "\\" [ '"' '\\' ]
      { Buffer.add_char string_buffer (lexeme_char lexbuf 1); string lexbuf }
  | eof
      { raise (Error(Unterminated "string", !string_start_pos)) }
  | _
      {  Buffer.add_char string_buffer (lexeme_char lexbuf 0); string lexbuf }

and comment = parse
  | "-->"
      { () }
  | eof
      { raise (Error(Unterminated "comment", !comment_start_pos)) }
  | _
      { comment lexbuf }

and base64 = parse
  | (space|break) +
      { base64 lexbuf }
  | ['A'-'Z']
      { Char.code (lexeme_char lexbuf 0) - Char.code 'A' }
  | ['a'-'z']
      { Char.code (lexeme_char lexbuf 0) - Char.code 'a' + 26 }
  | ['0'-'9']
      { Char.code (lexeme_char lexbuf 0) - Char.code '0' + 52 }
  | '+'
      { 62 }
  | '/'
      { 63 }
  | _
      { raise (Error(Illegal_character (lexeme_char lexbuf 0),
                     lexeme_start lexbuf)) }
