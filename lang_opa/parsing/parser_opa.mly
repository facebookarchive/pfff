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
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
/*
(* Some tokens below are not even used in this file because they are filtered
 * in some intermediate phases (e.g. the comment tokens).
 *)*/

/*(* unrecognized token, will generate parse error *)*/
%token <Ast_opa.tok> TUnknown

%token <Ast_opa.tok> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/

/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_opa.tok> TCommentSpace TCommentNewline   TComment
%token <Ast_opa.tok> TCommentMisc

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Ast_opa.tok> TInt
%token <string * Ast_opa.tok> TFloat
%token <string * Ast_opa.tok> TString
%token <string * Ast_opa.tok> T_ENCAPSED
%token <Ast_opa.tok> TGUIL
%token <string * Ast_opa.tok> TIdent TSharpIdent

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
 /*(* was not defined as keywords in manual, but used as keywords in grammar*)*/
 Tint Tstring Tfloat

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
 TTilde

/*(* operators *)*/

/*(* xml *)*/
%token <Ast_opa.tag * Ast_opa.tok> T_XML_OPEN_TAG
%token <Ast_opa.tag option * Ast_opa.tok> T_XML_CLOSE_TAG
%token <Ast_opa.attr * Ast_opa.tok> T_XML_ATTR
%token <Ast_opa.tok> T_XML_MORE
/*(* could be merged with T_ENCAPSED *)*/
%token <string * Ast_opa.tok> T_XML_TEXT

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc SHIFTHERE

%nonassoc Telse

%left TPlus TMinus
%left TStar TDiv


/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_opa.program> main

%%

/*(*************************************************************************)*/
/*(*1 TOC *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Toplevel, compilation units *)*/
/*(*************************************************************************)*/

main: program EOF { [] }

program: declaration_star { $1 }

declaration:
 | type_definition { }
 | binding { }
 | package_declaration { }
 | package_import { }
 | do_ { }

/*(*************************************************************************)*/
/*(*1 Names *)*/
/*(*************************************************************************)*/

long_ident:
 | TIdent { }
 | long_ident TDot TIdent { }

/*(* less: why they dont just use <ident> in grammar? *)*/
package_ident: TIdent { }

field: TIdent { }

/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(* TODO *)*/
type_:
 | Tint { }
 | Tstring { }
 | Tfloat { }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

/*(* TODO *)*/
expr:
 | literal { }
 | TIdent { }
 | record { }
 | Tif expr Tthen expr %prec SHIFTHERE { }
 | Tif expr Tthen expr Telse  expr { }
/* conflict
 | expr TColon type_ { }
*/
 | expr TPlus expr { }
 | expr TMinus expr { }
 | expr TStar expr { }
 | expr TDiv expr { }


literal:
 | TInt { }
 | TFloat { }
 | TString { }
 | TGUIL encap_star TGUIL { }

do_: Tdo expr { }

encap:
 | T_ENCAPSED { }
 | TOBrace expr TCBrace { }

record:
 | tilde_opt TOBrace record_field_star sc_opt TCBrace { }

record_field:
 |        field coerce_opt { }
 | TTilde field coerce_opt { }
 | field coerce_opt TEq expr { }

/*(*************************************************************************)*/
/*(*2 HTML *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*2 CSS *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*2 Pattern *)*/
/*(*************************************************************************)*/

/*(* TODO *)*/
pattern:
 | literal { }

/*(*************************************************************************)*/
/*(*1 Function/Var Binding *)*/
/*(*************************************************************************)*/

binding:
| non_rec_binding { }
| Trec rec_binding_plus { }

/*(* less: was just binding_directive in original grammar, weird *)*/
non_rec_binding:
| binding_directive_star ident_binding { }
| binding_directive_star val_binding { }

rec_binding:
 | ident_binding { }
 | Tval val_binding { }


/*(* less: was params+ in original grammar, weird *)*/
ident_binding:
 | TIdent TOParen pattern_params_star TCParen coerce_opt TEq expr { }
 | TIdent coerce_opt TEq expr { }

val_binding:
 | pattern TEq expr { }

pattern_params: pattern { }


binding_directive:
 | TAt    { }

coerce: TColon type_ { }

/*(*************************************************************************)*/
/*(*1 Type Binding *)*/
/*(*************************************************************************)*/

type_definition: Ttype { }

/*(*************************************************************************)*/
/*(*1 Module/Package *)*/
/*(*************************************************************************)*/

package_declaration: Tpackage package_ident { }

/*(* was package_expression_star in original grammar but weird *)*/
package_import: Timport package_expression_plus { }

package_expression:
 | package_ident { }
 | package_expression TStar { }
 | TOBrace package_expression_plus TCBrace { }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

declaration_star:
 | /*(*empty*)*/    { }
 | declaration_star declaration { }

encap_star:
 | /*(*empty*)*/    { }
 | encap_star encap { }

rec_binding_plus:
 | rec_binding { }
 | rec_binding_plus Tand rec_binding { }

package_expression_plus:
 | package_expression { }
 | package_expression_plus TComma package_expression { }

pattern_params_star:
 | /*(*empty*)*/    { }
 | pattern_params_star TComma pattern_params { }

record_field_star:
 | /*(*empty*)*/    { }
 | record_field_star TSemiColon record_field { }
 | record_field_star record_field { }

binding_directive_star:
 | /*(*empty*)*/    { }
 | binding_directive_star binding_directive { }

coerce_opt:
 | /*(*empty*)*/    { }
 | coerce { }

sc_opt:
 | /*(*empty*)*/    { }
 | TSemiColon { }

tilde_opt:
 | /*(*empty*)*/    { }
 | TTilde { }

