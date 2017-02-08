(*pp camlp4o -I `ocamlfind query ulex` pa_ulex.cma *)
(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


exception Error0 of string (* internally used error exception *)


let error s =
  raise (Error0 s)


let int_of_xstring s =
  int_of_string ("0x" ^ s)

let int_of_ostring s =
  (* TODO: check the range *)
  int_of_string ("0o" ^ s)


(* String_a is a subtype of both String_b and String_u *)
type string_type =
  | String_a (* string containing only 7-bit ascii characters *)
  | String_b (* string containing 8-bit binary data *)
  | String_u (* utf-8 encoded unicode string *)


(* find out whether the string is a utf8, ascii, or a binary string *)
let classify_string s =
  let res = ref String_a in (* assuming that it is an ascii string *)
  let len = String.length s in
  let rec aux i =
    if i = len
    then !res
    else begin
      let w = Utf8.width.(Char.code s.[i]) in
      if w > 0 && i + w <= len
      then (
        if w > 1 then res := String_u; (* width is > 1 => Utf8 *)
        (* check if the next unicode char is correctly encoded in utf8 *)
        ignore (Utf8.next s i);
        aux (i + w)
      )
      else raise Utf8.MalFormed
    end
  in
  try aux 0
  with Utf8.MalFormed -> String_b


let is_utf8_string s =
  classify_string s <> String_b


let type_of_char c =
  if c <= 127
  then String_a
  else String_b


let regexp digit = ['0'-'9']
let regexp odigit = ['0'-'7']
let regexp xdigit = ['0'-'9''a'-'f''A'-'F']


let make_char c =
  String_a, (Char.code c)


let escaped_lexeme lexbuf =
  (* strip the first symbol *)
  let start = Ulexing.get_start lexbuf in
  let pos = Ulexing.get_pos lexbuf in
  Ulexing.utf8_sub_lexeme lexbuf 1 (pos - start - 1)


(* XXX: add support for a b f v escapes? *)
let parse_string_escape = lexer
  | '\\' -> make_char '\\'
  | '"' -> make_char '"'
  | 't' -> make_char '\t'
  | 'n' -> make_char '\n'
  | 'r' -> make_char '\r'
  (* XXX: disable it for now, since specifying decimals this way may make more
   * sense:
  | odigit odigit odigit ->
      let c = int_of_ostring (Ulexing.utf8_lexeme lexbuf) in
      (type_of_char c),c
  *)
  | "x" xdigit xdigit ->
      let c = int_of_xstring (escaped_lexeme lexbuf) in
      (type_of_char c),c
  | 'u' xdigit xdigit xdigit xdigit ->
      let c = int_of_xstring (escaped_lexeme lexbuf) in
      String_u,c
  | 'U' xdigit xdigit xdigit xdigit xdigit xdigit xdigit xdigit ->
      (* XXX: check code validity so that it doesn't exeed allocated limit *)
      let c = int_of_xstring (escaped_lexeme lexbuf) in
      String_u,c
  | _ -> error "invalid string escape literal"


(* returns the list of integers representing codepoints *)
(* XXX: allow only printable characters in strings? *)
(* XXX: provide a method for wraping a string to several lines? *)
let rec parse_string_literal ltype l = lexer
  | '\\' ->
      let ctype, c = parse_string_escape lexbuf in
      let ltype =
        match ltype,ctype with
            String_a, _ ->
              (* set up string type to whatever character type is *)
              ctype
          (* TODO: print more meaningful error messages *)
          | String_b, String_u -> error "invalid string literal"
          | String_u, String_b -> error "invalid string literal"
          | _,_ -> ltype (* leave the previous type *)
        in
      parse_string_literal ltype (c::l) lexbuf
  | [0-0x1F 127] -> (* XXX: what about unicode non-printable chars? *)
      (* do not allow non-printables to appear in string literals -- one
       * should use correspondent escaped specifications instead *)
      error "invalid string literal" 
  | eof ->
      ltype,(List.rev l)
  | _ ->
      let c = Ulexing.lexeme_char lexbuf 0 in
      let ltype =
        match ltype with
           String_b when c > 127 -> error "invalid string literal" 
         | _ when c > 127 -> String_u (* upgrage string type to unicode *)
         | _ -> ltype
      in
      parse_string_literal ltype (c::l) lexbuf


let utf8_of_list l =
  let a = Array.of_list l in
  Utf8.from_int_array a 0 (Array.length a)


let string_of_list l =
  let s = String.create (List.length l) in
  let rec aux i = function
    | [] -> ()
    | h::t ->
        s.[i] <- Char.chr h; aux (i+1) t
  in
  aux 0 l; s


let parse_string_literal s =
  let lexbuf = Ulexing.from_utf8_string s in
  let str_type, l = parse_string_literal String_a [] lexbuf in
  let parsed_str =
    match str_type with
      | String_u -> utf8_of_list l
      | String_a | String_b -> string_of_list l
  in
  (str_type, parsed_str)


let add_ascii_char buf i =
  let add c = Buffer.add_char buf c in
  let add_escaped c = add '\\'; add c in
  let c = Char.chr i in
  match c with
    | '\n' -> add_escaped 'n'
    | '\r' -> add_escaped 'r'
    | '\t' -> add_escaped 't'
    | '"' | '\\' -> add_escaped c
    | _ when i >= 20 && i < 127 -> add c (* printable *)
    | _ ->
        add '\\';
        (* TODO: optimize *)
        Buffer.add_string buf (Printf.sprintf "x%02x" i)


(* escape utf8 string according to piq lexical conventions *)
let escape_string s =
  let len = String.length s in
  let a = Utf8.to_int_array s 0 len in
  let buf = Buffer.create (len + len / 10) in
  for i = 0 to Array.length a - 1
  do
    let c = a.(i) in
    if c <= 127
    then
      add_ascii_char buf c
    else
      (* XXX: check if unicode codepoint/sequence is printable and escape it
       * if it isn't *) 
      Utf8.store buf c
  done;
  Buffer.contents buf


(* escape binary string according to piq lexical conventions *)
(* NOTE: escaping each byte as hex value unless the character is
 * ASCII-printable *)
let escape_binary s =
  let len = String.length s in
  let buf = Buffer.create (len + len / 10) in
  for i = 0 to len - 1
  do
    let c = Char.code s.[i] in
    add_ascii_char buf c
  done;
  Buffer.contents buf


type token =
  | Lpar (* ( *)
  | Rpar (* ) *)
  | Lbr (* [ *)
  | Rbr (* ] *)
  | String of string_type * string * string (* ascii | utf8 | binary, parsed literal, original literal *)
  | Word of string
  | Text of string
  | EOF
  (* Raw binary -- just a sequence of bytes: may be parsed as either binary or
   * utf8 string
   *
   * NOTE: this is used only in several special cases, and can't be represented
   * in Piq text format directly *)
  | Raw_binary of string


let regexp newline = ('\n' | "\r\n")
let regexp ws = [' ' '\t']+


(* non-printable characters from ASCII range are not allowed
 * XXX: exclude Unicode non-printable characters as well? *)
let regexp word = [^'(' ')' '[' ']' '{' '}' '"' '%' '#' 0-0x20 127]+


(* accepts the same language as the regexp above *)
let is_valid_word s =
  let is_valid_char = function
    | '(' | ')' | '[' | ']' | '{' | '}'
    | '"' | '%' | '#' | '\000'..'\032' | '\127' -> false
    | _ -> true
  in
  let len = String.length s in
  (* NOTE: it works transparently on utf8 strings *)
  let rec check_chars i =
    if i >= len
    then true
    else
      if is_valid_char s.[i]
      then check_chars (i + 1)
      else false
  in
  if len = 0
  then false
  else is_utf8_string s && check_chars 0


type buf =
  {
    lexbuf : Ulexing.lexbuf;

    mutable lcount : int; (* line counter *)
    mutable lstart : int; (* buffer position of the latest line *)
    mutable col : int; (* column number of the last returned token *)

    mutable next_token : token option; (* rollback token *)
  }


let make_buf lexbuf =
  {
    lexbuf = lexbuf;
    lcount = 1;
    lstart = 0;
    col = 1;
    next_token = None;
  }


let update_line_counter buf =
  buf.lcount <- buf.lcount + 1;
  buf.lstart <- Ulexing.lexeme_end buf.lexbuf


let get_column buf = 
  (* NOTE: ennumerating columns from 1 *)
  (Ulexing.lexeme_start buf.lexbuf) - buf.lstart + 1


let update_column buf =
  buf.col <- get_column buf


(* location before we returned a token *)
let error_location buf =
  buf.lcount, get_column buf


(* valid location after we returned a token *)
let location buf =
  buf.lcount, buf.col


let rec token0 buf = lexer
  | newline ->
      (* update line counter, drop column counter and move on *)
      update_line_counter buf;
      token0 buf lexbuf 
  | ws -> token0 buf lexbuf (* skip whitespace *)
  | '\r' ->
      (* standalone '\r', i.e. without following '\n', is invalid *)
      error "invalid character"
  | "%%" ->
      error "'%%' literal is reserved for future versions"
  | '%' ( [^'%' '\n'] [^'\n']* )? newline? -> (* skip single line comment *)
      update_line_counter buf;
      token0 buf lexbuf 
  | '#' newline? -> (* verbatim empty text *)
      Text ""
  | '#' [^' '] ->
      error "space is expected after '#'"
  | '#' [' '] [^'\n']* newline? -> (* verbatim text *)
      (* TODO: restrict string literal to contain only printable characters *)
      let s = Ulexing.utf8_lexeme lexbuf in
      let len = String.length s in
      if len = 0
      then Text ""
      else
        let chomp =
          if s.[len-1] = '\n'
          then 1
          else if len > 1 && s.[len-2] = '\n' && s.[len-1] = '\r'
          then 2
          else 0
        in
        let s = String.sub s 2 (len - 2 - chomp) in (* cut #' ' and newline *)
        Text s

  | '(' -> Lpar
  | ')' -> Rpar
  | '[' -> Lbr
  | ']' -> Rbr
  | '"'([^'"']|"\\\"")*'"' -> (* string literal *)
      let s = Ulexing.utf8_lexeme lexbuf in
      let s = String.sub s 1 (String.length s - 2) in (* cut double-quotes *)

      let (str_type, parsed_str) = parse_string_literal s in
      String (str_type, parsed_str, s)

  | '"' -> error "string literal overrun"
  | word -> (* utf8 word delimited by other tokens or whitespace *)
      let s = Ulexing.utf8_lexeme lexbuf in
      Word s

  | eof -> EOF
  | _ -> error "invalid character"


(* error reporter *)
type loc = int * int (* line, column *)
exception Error of string * loc


let error buf s =
  raise (Error (s, error_location buf))


let token1 buf =
  try 
    let tok = token0 buf buf.lexbuf in
    update_column buf;
    (match tok with Text _ -> update_line_counter buf | _ -> ());
    tok
  with
    | Error0 s -> error buf s
    | Ulexing.Error -> error buf "lexing internal error"
    | Ulexing.InvalidCodepoint i -> 
        error buf ("invalid unicode code point " ^ string_of_int i)
    | Utf8.MalFormed ->
        error buf "malformed utf-8"


let rollback buf tok =
  buf.next_token <- Some tok


(* return next token *)
let token buf = 
  match buf.next_token with
    | None ->
        token1 buf
    | Some tok -> 
        buf.next_token <- None; tok


let init_from_string s =
  let lexbuf = Ulexing.from_utf8_string s in
  make_buf lexbuf


let init_from_stream s =
  let lexbuf = Ulexing.from_utf8_stream s in
  make_buf lexbuf


let init_from_channel ch =
  let lexbuf = Ulexing.from_utf8_channel ch in
  make_buf lexbuf


let tokenize_string s =
  let buf = init_from_string s in
  let rec aux accu = 
    match token buf with
      | EOF -> List.rev accu
      | t -> aux (t::accu)
  in
  aux []

