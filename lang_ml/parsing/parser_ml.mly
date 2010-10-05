/* Yoann Padioleau
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
 */
%{
(*
 * src: http://www.cs.ru.nl/~tews/htmlman-3.10/full-grammar.html
 * itself derived from the official ocaml reference manual.
 *)
%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_ml.info> TCommentSpace TCommentNewline   TComment
%token <Ast_ml.info> TCommentMisc

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Ast_ml.info> TInt
%token <string * Ast_ml.info> TFloat
%token <string * Ast_ml.info> TChar
%token <string * Ast_ml.info> TString

%token <string * Ast_ml.info> TLowerIdent
%token <string * Ast_ml.info> TUpperIdent
%token <string * Ast_ml.info> TLabelUse
%token <string * Ast_ml.info> TLabelDecl
%token <string * Ast_ml.info> TOptLabelUse
%token <string * Ast_ml.info> TOptLabelDecl

/*(* keywords tokens *)*/
%token <Ast_ml.info>
 Tfun Tfunction Trec
 Ttype Tof Tif Tthen Telse
 Tmatch Twith Twhen
 Tlet Tin
 Tas
 Ttry Texception
 Tbegin Tend
 Tfor Tdo Tdone Tdownto Twhile Tto
 Tval Texternal
 Ttrue Tfalse
 Tmodule Topen Tfunctor Tinclude Tsig Tstruct
 Tclass Tnew Tinherit Tconstraint Tinitializer Tmethod Tobject Tprivate
 Tvirtual
 Tlazy Tmutable Tassert
 Tand Tor Tmod Tlor Tlsl Tlsr Tlxor Tasr Tland

/*(* syntax *)*/
%token <Ast_ml.info> TOParen TCParen TOBrace TCBrace TOBracket TCBracket
%token <Ast_ml.info> TOBracketPipe TPipeCBracket
%token <Ast_ml.info> TOBracketLess TGreaterCBracket
%token <Ast_ml.info> TOBraceLess TGreaterCBrace

%token <Ast_ml.info> TOBracketGreater
%token <Ast_ml.info> TColonGreater

%token <Ast_ml.info> TLess TGreater

%token <Ast_ml.info> TDot TDotDot 
%token <Ast_ml.info> TComma
%token <Ast_ml.info> TEq
%token <Ast_ml.info> TAssign
%token <Ast_ml.info> TAssignMutable
%token <Ast_ml.info> TColon TColonColon
%token <Ast_ml.info> TBang TBangEq
%token <Ast_ml.info> TTilde
%token <Ast_ml.info> TPipe
%token <Ast_ml.info> TSemiColon TSemiColonSemiColon
%token <Ast_ml.info> TQuestion TQuestionQuestion
%token <Ast_ml.info> TUnderscore
%token <Ast_ml.info> TStar
%token <Ast_ml.info> TArrow
%token <Ast_ml.info> TQuote TBackQuote
%token <Ast_ml.info> TAnd TAndAnd
%token <Ast_ml.info> TSharp

%token <Ast_ml.info> TMinusDot

/*(* operators *)*/
%token <Ast_ml.info> TPlus TMinus
%token <Ast_ml.info> TLess TGreater
%token <string * Ast_ml.info> TPrefixOperator
%token <string * Ast_ml.info> TInfixOperator


/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

/*(* classic *)*/
%token <Ast_ml.info> TUnknown
%token <Ast_ml.info> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

/*(*-----------------------------------------*)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc SHIFTHERE

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start foo
%type <unit> foo

%%

/*(*************************************************************************)*/
/*(* Entry points *)*/
/*(*************************************************************************)*/

foo: EOF { () }


/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Names *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(* Qualified names *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(* Expressions *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(* Constants *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(* Patterns *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* Types *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(* Types definitions *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Exceptions *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Types expressions *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(* Classes *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(* Class types *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Class expressions *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Class definitions *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(* Modules *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(* Module types *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Module expressions *)*/
/*(*----------------------------*)*/


/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/
