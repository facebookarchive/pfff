%{
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

(*
 * http://doc.opalang.org/#!/manual/The-core-language
 *)
open Common

open Ast_opa

%}

/*(*************************************************************************)*/
/*(* tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_opa.tok> TCommentSpace TCommentNewline   TComment
%token <Ast_opa.tok> TCommentMisc

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Ast_opa.tok> TInt
%token <string * Ast_opa.tok> TFloat
%token <string * Ast_opa.tok> TString

%token <string * Ast_opa.tok> TIdent

/*(* keywords tokens *)*/
%token <Ast_opa.tok>
 Tif Tthen Telse
 Tmatch Tcase Tdefault
 Twith Tas
 Tfunction 
 Ttype Tor Tval Tand Trec
 Tbegin Tend
 Tcss Tdb Tparser Tserver
 Texternal
 Tforall
 Tpackage Tmodule Timport 
 Tpublic Tprivate   Tclient Tserver  Texposed Tprotected
 Tdo


/*(* syntax *)*/
%token <Ast_opa.tok> TOParen TCParen 
%token <Ast_opa.tok> TOBracket TCBracket
%token <Ast_opa.tok> TOBrace TCBrace

%token <Ast_opa.tok>
 TComma TColon TDot TSemiColon
 TArrow TUnderscore
 TStar TDiv /*(* TPercent *)*/
 TEq TNotEq TEqEq
 TPlus TMinus 
 TAnd TOr TXor
 TLess TMore /*(* TMoreEq TLessEq *)*/
 TQuestion TAt TAntiSlash TSharp
 TAndAnd TOrOr
 THat

/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/


/*(* classic *)*/
%token <Ast_opa.tok> TUnknown
%token <Ast_opa.tok> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/


/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_opa.program> main

%%

/*(*************************************************************************)*/
/*(* TOC *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

main: EOF { () }

/*(*************************************************************************)*/
/*(* Names *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Expressions *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Misc *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

