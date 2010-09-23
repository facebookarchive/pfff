type token =
  | STRING of (string)
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
# 2 "parser.mly"
  (* File: parser.mly

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

  (** Parser: Grammar Specification for Parsing S-expressions *)

  open Lexing

  let parse_failure what =
    let pos = symbol_start_pos () in
    let msg =
      Printf.sprintf "Sexplib.Parser: failed to parse line %d char %d: %s"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) what in
    failwith msg
# 44 "parser.ml"
let yytransl_const = [|
  258 (* LPAREN *);
  259 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\005\000\005\000\
\004\000\004\000\003\000\003\000\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\001\000\001\000\001\000\001\000\002\000\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\001\000\000\000\
\013\000\006\000\005\000\014\000\012\000\007\000\015\000\000\000\
\010\000\016\000\000\000\002\000\000\000\008\000\003\000"

let yydgoto = "\005\000\
\014\000\012\000\015\000\018\000\016\000"

let yysindex = "\010\000\
\030\255\001\000\004\000\007\000\000\000\000\000\000\000\015\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\255\
\000\000\000\000\030\255\000\000\019\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\005\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\008\000\000\000\000\000\000\000\254\255"

let yytablesize = 265
let yytable = "\000\000\
\010\000\019\000\011\000\013\000\009\000\021\000\017\000\000\000\
\009\000\011\000\001\000\002\000\003\000\004\000\006\000\007\000\
\008\000\020\000\006\000\007\000\008\000\023\000\000\000\022\000\
\000\000\000\000\022\000\000\000\022\000\006\000\007\000\008\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\007\000\008\000\006\000\007\000\008\000\006\000\007\000\
\008\000"

let yycheck = "\255\255\
\000\000\004\000\000\000\000\000\000\000\008\000\000\000\255\255\
\001\000\002\000\001\000\002\000\003\000\004\000\000\001\001\001\
\002\001\003\001\000\001\001\001\002\001\003\001\255\255\016\000\
\255\255\255\255\019\000\255\255\021\000\000\001\001\001\002\001\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\001\001\002\001\000\001\001\001\002\001\000\001\001\001\
\002\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
           ( Type.Atom _1 )
# 174 "parser.ml"
               : Type.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                  ( Type.List [] )
# 180 "parser.ml"
               : Type.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rev_sexps_aux) in
    Obj.repr(
# 58 "parser.mly"
                                ( Type.List (List.rev _2) )
# 187 "parser.ml"
               : Type.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
          ( parse_failure "sexp" )
# 193 "parser.ml"
               : Type.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Type.t) in
    Obj.repr(
# 62 "parser.mly"
         ( Some _1 )
# 200 "parser.ml"
               : Type.t option))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
        ( None )
# 206 "parser.ml"
               : Type.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Type.t) in
    Obj.repr(
# 66 "parser.mly"
         ( [_1] )
# 213 "parser.ml"
               : 'rev_sexps_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rev_sexps_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Type.t) in
    Obj.repr(
# 67 "parser.mly"
                       ( _2 :: _1 )
# 221 "parser.ml"
               : 'rev_sexps_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_sexps_aux) in
    Obj.repr(
# 70 "parser.mly"
                  ( _1 )
# 228 "parser.ml"
               : Type.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
        ( [] )
# 234 "parser.ml"
               : Type.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_sexps_aux) in
    Obj.repr(
# 74 "parser.mly"
                  ( List.rev _1 )
# 241 "parser.ml"
               : Type.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
        ( [] )
# 247 "parser.ml"
               : Type.t list))
(* Entry sexp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry sexp_opt *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry sexps *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry rev_sexps *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let sexp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Type.t)
let sexp_opt (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Type.t option)
let sexps (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Type.t list)
let rev_sexps (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Type.t list)
