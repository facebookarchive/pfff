# 1 "lexer.mll"
 
  (* File: lexer.mll

      Copyright (C) 2005-

        Jane Street Holding, LLC
        Author: Markus Mottl
        email: mmottl@janestcapital.com
        WWW: http://www.janestcapital.com/ocaml

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  *)

  (** Lexer: Lexer Specification for S-expressions *)

  open Printf
  open Lexing
  open Parser

  let char_for_backslash = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    | c   -> c

  let double_nl = "\013\010"

  let dec_code c1 c2 c3 =
    100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

  let hex_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48 in
    let d2 = Char.code c2 in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48 in
    val1 * 16 + val2

  let found_newline lexbuf diff =
    let curr_p = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        curr_p with
        pos_lnum = curr_p.pos_lnum + 1;
        pos_bol = max 1 (curr_p.pos_cnum - diff);
      }

  let get_lexeme_len lexbuf = lexbuf.lex_curr_pos - lexbuf.lex_start_pos

# 70 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\248\255\005\000\250\255\251\255\252\255\006\000\011\000\
    \001\000\255\255\011\000\015\000\245\255\001\000\002\000\016\000\
    \017\000\255\255\249\255\090\000\027\000\252\255\020\000\053\000\
    \022\000\047\000\251\255\113\000\250\255\013\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\006\000\255\255\255\255\255\255\255\255\001\000\
    \000\000\255\255\002\000\255\255\255\255\009\000\007\000\007\000\
    \255\255\255\255\255\255\006\000\006\000\255\255\001\000\001\000\
    \002\000\255\255\255\255\255\255\255\255\008\000";
  Lexing.lex_default = 
   "\002\000\000\000\002\000\000\000\000\000\000\000\010\000\255\255\
    \255\255\000\000\010\000\013\000\000\000\013\000\013\000\013\000\
    \018\000\000\000\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\013\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\009\000\009\000\007\000\008\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\007\000\255\255\000\000\007\000\
    \255\255\014\000\029\000\022\000\015\000\022\000\023\000\024\000\
    \007\000\000\000\003\000\255\255\255\255\255\255\000\000\255\255\
    \005\000\004\000\000\000\007\000\000\000\255\255\255\255\255\255\
    \000\000\017\000\255\255\021\000\022\000\000\000\024\000\000\000\
    \021\000\000\000\000\000\006\000\000\000\000\000\022\000\024\000\
    \255\255\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\022\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\255\255\000\000\016\000\255\255\021\000\000\000\000\000\
    \000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
    \000\000\000\000\000\000\021\000\000\000\021\000\000\000\000\000\
    \000\000\019\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\255\255\255\255\000\000\000\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\012\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\008\000\000\000\000\000\002\000\002\000\
    \006\000\002\000\002\000\006\000\007\000\010\000\255\255\007\000\
    \010\000\011\000\015\000\016\000\011\000\022\000\016\000\024\000\
    \000\000\255\255\000\000\013\000\014\000\002\000\255\255\002\000\
    \000\000\000\000\255\255\007\000\255\255\002\000\002\000\029\000\
    \255\255\011\000\015\000\016\000\022\000\255\255\024\000\255\255\
    \016\000\255\255\255\255\000\000\255\255\255\255\023\000\023\000\
    \002\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\023\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\013\000\014\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\029\000\255\255\011\000\015\000\016\000\255\255\255\255\
    \255\255\255\255\255\255\016\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\016\000\
    \255\255\255\255\255\255\016\000\255\255\016\000\255\255\255\255\
    \255\255\016\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\019\000\019\000\019\000\019\000\019\000\
    \019\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\027\000\027\000\027\000\027\000\027\000\027\000\
    \255\255\255\255\255\255\019\000\019\000\019\000\019\000\019\000\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\027\000\027\000\027\000\027\000\027\000\027\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\013\000\014\000\255\255\255\255\002\000\006\000\255\255\
    \255\255\255\255\255\255\010\000\255\255\029\000\255\255\011\000\
    \015\000\016\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec main buf lexbuf =
    __ocaml_lex_main_rec buf lexbuf 0
and __ocaml_lex_main_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 75 "lexer.mll"
            ( found_newline lexbuf 1; main buf lexbuf )
# 204 "lexer.ml"

  | 1 ->
# 76 "lexer.mll"
           ( main buf lexbuf )
# 209 "lexer.ml"

  | 2 ->
# 77 "lexer.mll"
                       ( main buf lexbuf )
# 214 "lexer.ml"

  | 3 ->
# 78 "lexer.mll"
        ( LPAREN )
# 219 "lexer.ml"

  | 4 ->
# 79 "lexer.mll"
        ( RPAREN )
# 224 "lexer.ml"

  | 5 ->
# 81 "lexer.mll"
      (
        scan_string buf lexbuf;
        let str = Buffer.contents buf in
        Buffer.clear buf;
        STRING str
      )
# 234 "lexer.ml"

  | 6 ->
let
# 87 "lexer.mll"
                                           str
# 240 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 87 "lexer.mll"
                                               ( STRING str )
# 244 "lexer.ml"

  | 7 ->
# 88 "lexer.mll"
        ( EOF )
# 249 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_main_rec buf lexbuf __ocaml_lex_state

and scan_string buf lexbuf =
    __ocaml_lex_scan_string_rec buf lexbuf 11
and __ocaml_lex_scan_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 91 "lexer.mll"
        ( () )
# 260 "lexer.ml"

  | 1 ->
# 93 "lexer.mll"
      (
        let len = get_lexeme_len lexbuf in
        found_newline lexbuf (len - 2);
        scan_string buf lexbuf
      )
# 269 "lexer.ml"

  | 2 ->
# 99 "lexer.mll"
      (
        let len = get_lexeme_len lexbuf in
        found_newline lexbuf (len - 3);
        scan_string buf lexbuf
      )
# 278 "lexer.ml"

  | 3 ->
let
# 104 "lexer.mll"
                               c
# 284 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 105 "lexer.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf lexbuf
      )
# 291 "lexer.ml"

  | 4 ->
let
# 109 "lexer.mll"
                       c1
# 297 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 109 "lexer.mll"
                                         c2
# 302 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 109 "lexer.mll"
                                                            c3
# 307 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 110 "lexer.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let pos = lexbuf.lex_curr_p in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Buffer.add_char buf (Char.chr v);
        scan_string buf lexbuf
      )
# 324 "lexer.ml"

  | 5 ->
let
# 124 "lexer.mll"
                                           c1
# 330 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 124 "lexer.mll"
                                                                             c2
# 335 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 125 "lexer.mll"
      (
        let v = hex_code c1 c2 in
        if v > 255 then (
          let pos = lexbuf.lex_curr_p in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\x%c%c'"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol - 3)
              c1 c2 in
          failwith msg);
        Buffer.add_char buf (Char.chr v);
        scan_string buf lexbuf
      )
# 352 "lexer.ml"

  | 6 ->
let
# 139 "lexer.mll"
               c
# 358 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 140 "lexer.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        scan_string buf lexbuf
      )
# 366 "lexer.ml"

  | 7 ->
let
# 145 "lexer.mll"
                       c
# 372 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 146 "lexer.mll"
      (
        found_newline lexbuf 1;
        Buffer.add_char buf c;
        scan_string buf lexbuf
      )
# 380 "lexer.ml"

  | 8 ->
# 152 "lexer.mll"
      (
        found_newline lexbuf 2;
        Buffer.add_string buf double_nl;
        scan_string buf lexbuf
      )
# 389 "lexer.ml"

  | 9 ->
# 158 "lexer.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;
        scan_string buf lexbuf
      )
# 399 "lexer.ml"

  | 10 ->
# 164 "lexer.mll"
        ( failwith "Sexplib.Lexer.scan_string: unterminated string" )
# 404 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_scan_string_rec buf lexbuf __ocaml_lex_state

;;

# 166 "lexer.mll"
 
  let main ?buf =
    let buf =
      match buf with
      | None -> Buffer.create 64
      | Some buf -> Buffer.clear buf; buf
    in
    main buf

# 420 "lexer.ml"
