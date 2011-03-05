/* Yoann Padioleau
 * 
 * Copyright (C) 2011 Facebook
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
open Common

(*************************************************************************)
(* Prelude *)
(*************************************************************************)

(* 
 * This file is used only to define the tokens. There is no actuall
 * grammar in this file. One could move the token definitions directly
 * in lexer_html.mll, as it was done originally in ocamlnet/netstring,
 * but we could at some point define a grammar. Moreover it is more
 * symetric with what we do in the other lang_xxx/ directories.
 *)
%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

%token <Ast_html.info> TComment  /*(* <!-- ... --> *)*/
%token <Ast_html.info> TDoctype  /*(* <! ... > *)*/
%token <Ast_html.info> TPi       /*(* <? ... ?> or >  *)*/

%token <Ast_html.info * string>   Lelement
%token <Ast_html.info * string>   Lelementend
%token <Ast_html.info> Relement  /*(* > *)*/
%token <Ast_html.info> Relement_empty   /*(* />, for XML compat *)*/
%token <Ast_html.info * string> Cdata
%token <Ast_html.info * string> Space
%token <Ast_html.info * string> Name
%token <Ast_html.info> Is
%token <Ast_html.info * string> Literal
%token <Ast_html.info> Other

/*(*-----------------------------------------*)*/
%token <Ast_html.info> EOF


/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <unit> main

%%

main: EOF { }
