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

%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(* the comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_nw.info> TComment
%token <Ast_nw.info> TCommentSpace TCommentNewline   

/*(*-----------------------------------------*)*/
/*(* the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with a value *)*/

%token<string * Ast_nw.info> TCommand
%token<string * Ast_nw.info> TWord
%token<string * Ast_nw.info> TSymbol
%token<string * Ast_nw.info> TNumber

/*(* keywords tokens *)*/
%token<Ast_nw.info> TBeginVerbatim TEndVerbatim
%token<string * Ast_nw.info> TVerbatimLine


%token<Ast_nw.info> TBeginNowebChunk TEndNowebChunk
%token<string * Ast_nw.info> TNowebChunkLine

/*(* syntax *)*/
%token <Ast_nw.info>  
  TOBrace TCBrace

/*(* operators *)*/

/*(*-----------------------------------------*)*/
/*(* extra tokens: *)*/
/*(*-----------------------------------------*)*/


/*(* classic *)*/
%token <Ast_nw.info> TUnknown
%token <Ast_nw.info> EOF

/*(*-----------------------------------------*)*/
/*(* priorities *)*/
/*(*-----------------------------------------*)*/

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <Ast_nw.toplevel list> main

%%

/*(*************************************************************************)*/
/*(* Toplevel *)*/
/*(*************************************************************************)*/

main: EOF { [] }

