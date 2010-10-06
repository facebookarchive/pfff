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
 *  itself probably derived from the official ocaml reference manual
 *  (note that it unfortunately contains conflict when translated into yacc).
 * src: http://www.mpi-sws.org/~rossberg/sml.html
 *  (note that it also contains conflict when translated into yacc).
 * src: http://www.mpi-sws.org/~rossberg/hamlet/
 *  solves ambiguities
 * 
 * alternatives: use menhir ? use dypgen ?
 *)
open Common

open Ast_ml

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

%start unit_interface  unit_implementation
%type <Ast_ml.toplevel list> unit_interface
%type <Ast_ml.toplevel list> unit_implementation

%%

/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

unit_interface: specification_list_opt EOF { $1 ++ [FinalDef $2] }

unit_implementation: definition_list_opt EOF { $1 ++ [FinalDef $2] }




specification: 
 | Tval      value_name TColon typexpr { TODO $1 }
 | Texternal value_name TColon typexpr TEq external_declaration { TODO $1 }
 | type_definition { $1 }
 | Texception constr_decl { TODO $1 }

definition: 
 | Tlet rec_opt let_binding /*TODOand_let_binding_list_opt*/ { TODO $1 }


let_binding:
 | TEq  { }
/*
 | pattern TEq expr { }
 | value_name parameter_list_opt type_annot_opt TEq expr { }
*/

/* TODO: optional ;; */
specification_list_opt:
 | /* empty */ { [] }
 | specification_list { $1 }
specification_list:
 | specification { [$1] }
 | specification_list specification { $1 ++ [$2] }

definition_list_opt:
 | /* empty */     { [] }
 | definition_list { $1 }
definition_list:
 | definition { [$1] }
 | definition_list definition { $1 ++ [$2] }

/*(*************************************************************************)*/
/*(* Names *)*/
/*(*************************************************************************)*/

value_name:
 | TLowerIdent { }
 | TOParen operator_name TCParen { }

operator_name:
 | TPrefixOperator { }
 | infix_op { }

infix_op:
 | TInfixOperator { }
 | TStar { }
 | TEq { }
 | Tor { }
 /*(* but not Tand, because of conflict ? *)*/
 | TAnd { } 
 | TAssign { }

 | Tmod { }
 | Tland { }
 | Tlor { }
 | Tlxor { }
 | Tlsl { } 
 | Tlsr { }
 | Tasr { }


typeconstr_name: TLowerIdent { }

module_name: TUpperIdent { }
 
/*(*----------------------------*)*/
/*(* Qualified names *)*/
/*(*----------------------------*)*/

module_path:
 | module_name { }
 | module_path TDot module_name { }

extended_module_path:
 | module_name { }
 | extended_module_path TDot module_name { }
 | extended_module_path TOParen extended_module_path TCParen { }




typeconstr_path:
 | typeconstr_name { }
 | extended_module_path typeconstr_name { }

/*(*----------------------------*)*/
/*(* Misc names *)*/
/*(*----------------------------*)*/

/*(* used in typexpr as both 'a and 'A are valid polymorphic names *)*/
ident: 
 | TLowerIdent { }
 | TUpperIdent { }


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

/*(* src: http://www.mpi-sws.org/~rossberg/hamlet/ disambiguation tricks *)*/
typexpr:
 | ty_tuple TArrow typexpr { }
 | ty_tuple { }

ty_tuple:
 | ty_star_list { }

ty_star_list:
 | ty_cons { }
 | ty_cons TStar ty_star_list { }

ty_cons:
 | ty_simple { }
 | ty_seq typeconstr_path { }

ty_simple:
 | typeconstr_path { }
 | TQuote ident { }
 | TOParen typexpr TCParen { }

ty_seq:
 | ty_cons { }
 | TOParen ty_comma_list TCParen { }

ty_comma_list:
 | typexpr TComma ty_comma_list { }
 | typexpr TComma typexpr { }

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
/*(* Misc *)*/
/*(*************************************************************************)*/

external_declaration: TString { }

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

rec_opt:
 | Trec { }
 | /*(*empty*)*/ { }


