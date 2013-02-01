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
open Common

open Ast_csharp

%}

/*(*************************************************************************)*/
/*(* tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_csharp.info> TCommentSpace TCommentNewline   TComment
%token <Ast_csharp.info> TCommentMisc

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Ast_csharp.info> TInt
%token <string * Ast_csharp.info> TFloat
%token <string * Ast_csharp.info> TChar
%token <string * Ast_csharp.info> TString

%token <string * Ast_csharp.info> TIdent

/*(* keywords tokens *)*/
%token <Ast_csharp.info>
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
%token <Ast_csharp.info>
  TCppLine TCppError TCppWarning 
  TCppRegion TCppEndRegion
  TDefine TUndef
  TIfdefIf TIfdefElif TIfdefElse TIfdefEndif

/*(* syntax *)*/
%token <Ast_csharp.info> TOParen TCParen 
%token <Ast_csharp.info> TOBracket TCBracket
%token <Ast_csharp.info> TOBrace TCBrace
%token <Ast_csharp.info> TOAngle TCAngle


%token <Ast_csharp.info>
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

%token <string * Ast_csharp.info> TAssignOp


/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

/*(* classic *)*/
%token <Ast_csharp.info> TUnknown
%token <Ast_csharp.info> EOF

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

