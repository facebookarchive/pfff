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
 * http://www.jaggersoft.com/csharp_grammar.html
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
%token <string * Parse_info.info> TChar
%token <string * Parse_info.info> TString

%token <string * Parse_info.info> TIdent

/*(* keywords tokens *)*/
%token <Parse_info.info>
  Tbool Tbyte Tchar Tvoid Tdouble Tfloat Tshort Tint Tlong Tstring Tsbyte
  Tushort Tuint Tulong
  Tclass Tabstract Tvirtual Tdelegate Tthis   Tinterface Tnew Tobject
  Tprivate Tprotected Tpublic
  Treturn
  Tbreak Tcontinue
  Tswitch Tcase Tdefault
  Tenum   Tstruct
  Tconst
  Tunsafe
  Tnamespace Tusing
  Tstatic Tvolatile Textern
  Tif Telse
  Tdo  Twhile
  Tfor Tforeach
  Tgoto
  Tthrow  Ttry  Tcatch  Tfinally
  Tchecked Tunchecked 
  Tnull
  Ttrue Tfalse
  Tref Tout

  Tas Tbase  Tdecimal Tevent Texplicit Tfixed Timplicit
  Tin Tinternal Tis Tlock
  Toperator  Toverride Tparams Treadonly 
  Tsealed Tsizeof Tstackalloc  Ttypeof

/*(* cpp *)*/
%token <Parse_info.info>
  TCppLine TCppError TCppWarning 
  TCppRegion TCppEndRegion
  TDefine TUndef
  TIfdefIf TIfdefElif TIfdefElse TIfdefEndif

/*(* syntax *)*/
%token <Parse_info.info> TOParen TCParen 
%token <Parse_info.info> TOBracket TCBracket
%token <Parse_info.info> TOBrace TCBrace
%token <Parse_info.info> TOAngle TCAngle


%token <Parse_info.info>
 TComma TColon TDot TSemiColon
 TStar TDiv TPercent
 TEq TEqEq TNotEq
 TPlus TMinus 
 TTilde
 TAnd TOr TXor
 TLess TMore TMoreEq TLessEq
 TQuestion
 TInc TDec
 TBang TTilde
 TAndAnd TOrOr
 TArrow

%token <string * Parse_info.info> TAssignOp


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
%type <Ast_csharp.program> main

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

