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
%token <string * Ast_opa.tok> TChar
%token <string * Ast_opa.tok> TString

%token <string * Ast_opa.tok> TIdent

/*(* keywords tokens *)*/
%token <Ast_opa.tok>
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
%token <Ast_opa.tok>
  TCppLine TCppError TCppWarning 
  TCppRegion TCppEndRegion
  TDefine TUndef
  TIfdefIf TIfdefElif TIfdefElse TIfdefEndif

/*(* syntax *)*/
%token <Ast_opa.tok> TOParen TCParen 
%token <Ast_opa.tok> TOBracket TCBracket
%token <Ast_opa.tok> TOBrace TCBrace
%token <Ast_opa.tok> TOAngle TCAngle


%token <Ast_opa.tok>
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

%token <string * Ast_opa.tok> TAssignOp


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

