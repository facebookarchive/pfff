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
%token <Parse_info.info> TCommentSpace TCommentNewline   TComment
%token <Parse_info.info> TCommentMisc

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Parse_info.info> TInt
%token <string * Parse_info.info> TFloat
%token <string * Parse_info.info> TComplex
%token <string * Parse_info.info> TChar
%token <string * Parse_info.info> TString
%token <string * Parse_info.info> TLongString

%token <string * Parse_info.info> TIdent

/*(* keywords tokens *)*/
%token <Parse_info.info>
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
%token <Parse_info.info> TOParen TCParen 
%token <Parse_info.info> TOBracket TCBracket
%token <Parse_info.info> TOBrace TCBrace
%token <Parse_info.info> TOAngle TCAngle

%token <Parse_info.info>
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

%token <string * Parse_info.info>
 TAugOp
 

/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/


/*(* classic *)*/
%token <Parse_info.info> TUnknown
%token <Parse_info.info> EOF


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

