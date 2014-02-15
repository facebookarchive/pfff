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
 * We support only the js-like syntax.
 * 
 * Note that the original syntax can not be parsed with Yacc;
 * function calls and local function definitions are too similar.
 * I've also decided to not use the %left directives and manually
 * encode the associativity/priorities of operators because of some
 * ambiguities I was not able to resolve when adding other rules
 * (e.g. local bindings).
 *)

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
%token <string * Ast_opa.tok> T_ENCAPSED
%token <Ast_opa.tok> TGUIL
%token <string * Ast_opa.tok> TIdent
%token <string * Ast_opa.tok> TSharpIdent
%token <string * Ast_opa.tok> TTypeVar 
%token <string * Ast_opa.tok> TOp
%token <string * Ast_opa.tok> TExternalIdent

/*(* keywords tokens *)*/
%token <Ast_opa.tok>
 Tif Tthen Telse
 Tmatch Tcase Tdefault
 Twith Tas
 Tfunction 
 Ttype Tor Tval Tand Trec
 Tbegin Tend
 Tcss Tdatabase Tparser
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

/*(* html *)*/
%token <Ast_opa.tag * Ast_opa.tok> T_XML_OPEN_TAG
%token <Ast_opa.tag option * Ast_opa.tok> T_XML_CLOSE_TAG
%token <Ast_opa.attr * Ast_opa.tok> T_XML_ATTR
%token <Ast_opa.tok> T_XML_MORE T_XML_SLASH_GT
/*(* could be merged with T_ENCAPSED *)*/
%token <string * Ast_opa.tok> T_XML_TEXT

/*(* css, todo: actually parse this, use lang_css/ ? *)*/
%token <Ast_opa.tok> T_CSS_TEXT

/*(* parser, todo: actually parse this *)*/
%token <Ast_opa.tok> T_PARSER_BEFORE_ARROW

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc LOW_PRIORITY_RULE

%nonassoc Telse

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

program:
 | declaration_plus { $1 }
 | expr { $1 }

declaration:
 | type_definition { }
 | binding { }
 | package_declaration { }
 | package_import { }

/*(*************************************************************************)*/
/*(*1 Names *)*/
/*(*************************************************************************)*/

/*
long_ident:
 | TIdent { }
 | long_ident TDot TIdent { }
*/

/*(* less: why they dont just use <ident> in reference manual? *)*/
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

 | TIdent { }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

/*(* TODO *)*/
expr:
 | ident_binding cond_expr { }
 | ident_binding TSemiColon cond_expr { }
 | cond_expr { }

cond_expr:
 | Tif comp_expr Tthen cond_expr Telse cond_expr      { }
 | Tif comp_expr Tthen cond_expr %prec LOW_PRIORITY_RULE { }
 | comp_expr { }

comp_expr:
 | comp_expr TEqEq arith_expr { }
 | arith_expr { }

arith_expr:
 | arith_expr TPlus term { }
 | arith_expr TMinus term { }
 | term { }

term: 
 | term TStar factor { }
 | term TDiv factor { }
 | factor { }

factor:
 | factor TOp type_expr { }
 | type_expr { }

type_expr:
 | call_expr coerce { }
 | call_expr { }

call_expr:
 | field_expr TOParen TCParen { }
 | field_expr TOParen expr_plus_comma TCParen { }
 | field_expr { }

field_expr: 
 | primitive_expr TDot field { }
 | primitive_expr { }

primitive_expr:
 | literal { }
 | TIdent { }
 | record { }
 | tuple { }
 | list { }
 /*(* was called grouping *)*/
 | TOParen expr TCParen { }
 | Tbegin expr Tend     { }
 | lambda { }

literal:
 | TInt { }
 | TFloat { }
 | TGUIL encap_star TGUIL { }

encap:
 | T_ENCAPSED { }
 | TOBrace expr TCBrace { }

lambda: 
 | Tfunction TOParen TCParen coerce_opt
    TOBrace expr TCBrace { }
 | Tfunction TOParen pattern_params_plus TCParen coerce_opt
    TOBrace expr TCBrace { }

/*(*----------------------------*)*/
/*(*2 composed expressions      *)*/
/*(*----------------------------*)*/

/*(* conflict: had to specialize TOBrace TCBrace empty case instead
   * of using record_field_star. Same reason I don't use tilde_opt
   * and duplicate rules with TTilde.
   *)*/
record:
 |        TOBrace TCBrace { }
 |        TOBrace record_field_plus TCBrace { }
 | TTilde TOBrace record_field_plus TCBrace { }
 |        TOBrace expr_with Twith record_field_with_plus TCBrace { }
 | TTilde TOBrace expr_with Twith record_field_with_plus TCBrace { }

record_field:
 |        field  { }
 | TTilde field  { }
 /*(* js-syntax: use : instead of = *)*/
 | field TColon expr { }

/*(* mostly dupe of record_field but also allow the with a.c = ... *)*/
record_field_with:
 |        field  { }
 | TTilde field  { }
 /*(* js-syntax: use : instead of = *)*/
 | field  TColon expr { }
 | field_long TEq expr { }

expr_with: TIdent { }

field_long: TIdent TDot ident_plus { }
ident_plus: 
 | TIdent { }
 | ident_plus TDot TIdent { }

tuple:
 | TOParen expr TComma TCParen { }
 | TOParen expr TComma expr_plus_comma comma_opt TCParen { }

/*(* conflict: had to specialize [] empty case again *)*/
list:
 | TOBracket TCBracket { }
 | TOBracket expr_plus_comma TCBracket { }
 | TOBracket expr_plus_comma TOr expr TCBracket { }


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
 | TIdent { }

/*(*************************************************************************)*/
/*(*1 Function/Var Binding *)*/
/*(*************************************************************************)*/

binding:
| non_rec_binding { }
| Trec rec_binding_plus { }

/*(* less: was just binding_directive in original grammar, weird 
   * todo: binding_directive_star generates conflict with 'expr: binding expr'
   *)*/
non_rec_binding:
| ident_binding { }
/* conflict with toplevel expr
| val_binding { }
*/

rec_binding:
 | ident_binding { }
 | Tval val_binding { }


/*
 (* less: was params+ in original grammar because of curried syntax support.
  * Note that expr can contain nested bindings too.
  * conflict: can't use coerce_opt, conflict when add  expr: binding expr.
  * conflict: The old syntax is not parseable with yacc I think;
  * local function definitions and function calls start the same way.
  * Similar to the problem of decl vs expr in C++ I think.
  *)*/
ident_binding:
 | Tfunction TIdent TOParen TCParen coerce_opt 
    TOBrace expr TCBrace { }
 | Tfunction TIdent TOParen pattern_params_plus TCParen coerce_opt
    TOBrace expr TCBrace { }
 | TIdent TEq expr { }
 | TIdent coerce TEq expr { }

val_binding:
 | pattern TEq expr { }


pattern_params: pattern { }

/*
binding_directive:
 | TAt    { }
*/

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

declaration_plus:
 | declaration    { }
 | declaration_plus declaration { }

encap_star:
 | /*(*empty*)*/    { }
 | encap_star encap { }

rec_binding_plus:
 | rec_binding { }
 | rec_binding_plus Tand rec_binding { }

package_expression_plus:
 | package_expression { }
 | package_expression_plus TComma package_expression { }

pattern_params_plus:
 | pattern_params    { }
 | pattern_params_plus TComma pattern_params { }

expr_plus_comma:
 | expr    { }
 | expr_plus_comma TComma expr { }


/*(* can't use it, generate conflicts *)*/
/*
record_field_star:
 | { }
 | record_field_star TSemiColon record_field { }
 | record_field_star record_field { }
*/

 /*(* js-syntax: use , instead of ; *)*/
record_field_plus:
 | record_field   { }
 | record_field_plus TComma record_field { }
 | record_field_plus record_field { }

record_field_with_plus:
 | record_field_with   { }
 | record_field_with_plus TComma record_field_with { }
 | record_field_with_plus record_field_with { }

/*
binding_directive_star:
 | { }
 | binding_directive_star binding_directive { }
*/

coerce_opt:
 |  { }
 | coerce { }

/*
sc_opt:
 | { }
 | TSemiColon { }
*/

comma_opt:
 | /*(*empty*)*/    { }
 | TComma { }

/*(* can't use it, generate conflicts *)*/
/*
tilde_opt:
 |     { }
 | TTilde { }
*/
