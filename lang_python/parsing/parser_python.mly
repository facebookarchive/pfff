%{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
 * src: http://inst.eecs.berkeley.edu/~cs164/sp10/python-grammar.html
 * src: http://docs.python.org/release/2.5.2/ref/grammar.txt
 *)

%}


/*(*************************************************************************)*/
/*(* tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_python.info> TCommentSpace TCommentNewline   TComment
%token <Ast_python.info> TCommentMisc

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Ast_python.info> TInt
%token <string * Ast_python.info> TFloat
%token <string * Ast_python.info> TComplex
%token <string * Ast_python.info> TChar
%token <string * Ast_python.info> TString
%token <string * Ast_python.info> TLongString

%token <string * Ast_python.info> TIdent

/*(* keywords tokens *)*/
%token <Ast_python.info>
  Tdef Tlambda Tclass
  Tif Telse Telif
  Twhile Tfor
  Treturn
  Tyield   Tbreak Tcontinue
  Ttry Traise Tfinally 
  Tor Tand Tnot
  Tglobal
  Tdel Tfrom Tas Twith Tassert Tpass Texcept Timport Tprint Texec Tin Tis

/*(* syntax *)*/
%token <Ast_python.info> TOParen TCParen 
%token <Ast_python.info> TOBracket TCBracket
%token <Ast_python.info> TOBrace TCBrace
%token <Ast_python.info> TOAngle TCAngle

%token <Ast_python.info>
 TComma
 TColon
 TBackQuote
 TDot
 TEllipsis
 TStar TStarStar
 TEq
 TPlus TMinus 
 TTilde
 TSlash  TSlashSlash
 TPercent
 TAnd TOr TXor
 TLess TMore TEqEq TMoreEq TLessEq TDiff TNotEq
 TAt

%token <string * Ast_python.info>
 TAugOp
 

/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/


/*(* classic *)*/
%token <Ast_python.info> TUnknown
%token <Ast_python.info> EOF


/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_python.program> main

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
/*(* Classes *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Misc *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

