/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/
/* Yoann Padioleau */
%{
(* 
 * src: slightly adapted from the official source of OCaml in its
 *  parsing/ subdirectory.
 * 
 * was: $Id: parser.mly 10536 2010-06-07 15:32:32Z doligez $
 *
 * other sources:
 * - http://caml.inria.fr/pub/docs/manual-ocaml/language.html
 *  (note that it unfortunately contains conflict when translated into yacc).
 * - http://www.cs.ru.nl/~tews/htmlman-3.10/full-grammar.html
 *  itself derived from the official ocaml reference manual
 *  (note that it also contains conflict when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/sml.html
 *  (note that it also contains conflict when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/hamlet/
 *  solves ambiguities
 * - linear ML parser
 * 
 * alternatives: 
 *   - use menhir ? 
 *   - use dypgen ?
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

/*
(* Precedences and associativities.
 *
 * Tokens and rules have precedences.  A reduce/reduce conflict is resolved
 * in favor of the first rule (in source file order).  A shift/reduce conflict
 * is resolved by comparing the precedence and associativity of the token to
 * be shifted with those of the rule to be reduced.
 * 
 * By default, a rule has the precedence of its rightmost terminal (if any).
 * 
 * When there is a shift/reduce conflict between a rule and a token that
 * have the same precedence, it is resolved using the associativity:
 * if the token is left-associative, the parser will reduce; if
 * right-associative, the parser will shift; if non-associative,
 * the parser will declare a syntax error.
 * 
 * We will only use associativities with operators of the kind  x * x -> x
 * for example, in the rules of the form    expr: expr BINOP expr
 * in all other cases, we define two precedences if needed to resolve
 * conflicts.
 * 
 * The precedences must be listed from low to high.
 *)*/

%nonassoc Tin
%nonassoc below_SEMI
%nonassoc TSemiColon                     /* below TEq ({lbl=...; lbl=...}) */
%nonassoc Tlet                           /* above TSemiColon ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc Tfunction Twith                 /* below TPipe  (match ... with ...) */
%nonassoc TAnd             /* above Twith (module rec A: Tsig with ... and ...) */
%nonassoc Tthen                          /* below Telse (if ... then ...) */
%nonassoc Telse                          /* (if ... then ... else ...) */
%nonassoc TAssignMutable                 /* below TAssign (lbl <- x := e) */
%right    TAssign                        /* expr (e := e := e) */
%nonassoc Tas
%left     TPipe                          /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     TComma                         /* expr/expr_comma_list (e,e,e) */
%right    TArrow                         /* core_type2 (t -> t -> t) */
%right    Tor BARBAR                     /* expr (e || e || e) */
%right    Tand TAndAnd                   /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 TEq TLess TGreater    /* expr (e OP e OP e) */
%right    INFIXOP1                       /* expr (e OP e OP e) */
%right    TColonColon                    /* expr (e :: e :: e) */
%left     INFIXOP2 TPlus PLUSDOT TMinus TMinusDot  /* expr (e OP e OP e) */
%left     INFIXOP3 TStar                 /* expr (e OP e OP e) */
%right    INFIXOP4                       /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor      /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl               /* above Tas TPipe TColonColon TComma */
%nonassoc below_SHARP
%nonassoc TSharp                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc TDot
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc TBackQuote TBang Tbegin TChar Tfalse TFloat TInt INT32 INT64
          TOBrace TOBraceLess TOBracket TOBracketPipe TLowerIdent TOParen
          Tnew NATIVEINT PREFIXOP TString Ttrue TUpperIdent

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start interface  implementation
%type <Ast_ml.toplevel list> interface
%type <Ast_ml.toplevel list> implementation

%%

/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

implementation: structure EOF                        { [FinalDef $2] }

interface:      signature EOF                        { [FinalDef $2] }


/*(*************************************************************************)*/
/*(* Structure *)*/
/*(*************************************************************************)*/

structure: Texception { }

/*(*************************************************************************)*/
/*(* Signature *)*/
/*(*************************************************************************)*/

signature:
 | /* empty */                                 { }
 | signature signature_item                    { }
 | signature signature_item TSemiColonSemiColon           { }

signature_item:
 | Tval val_ident TColon core_type
     { }
 | Texternal val_ident TColon core_type TEq primitive_declaration
     { }
 | Ttype type_declarations
      { }


primitive_declaration:
 | TString                                      { }
 | TString primitive_declaration                { }

/*(*************************************************************************)*/
/*(* Names *)*/
/*(*************************************************************************)*/

val_ident:
 | TLowerIdent                                      { }
 | TOParen operator TCParen                      { }

operator:
 | TPrefixOperator                                    { }
 | TInfixOperator { }
 | TStar                                        { }
 | TEq                                       { }
 | Tor                                          { }
  /*(* but not Tand, because of conflict ? *)*/
 | Tand                                   { }
 | TAssign                                  { }
 | Tmod { }
 | Tland { }
 | Tlor { }
 | Tlxor { }
 | Tlsl { } 
 | Tlsr { }
 | Tasr { }


/*(* for polymorphic types both 'a and 'A is valid *)*/
ident:
 | TUpperIdent                                      { }
 | TLowerIdent                                      { }


constr_ident:
 | TUpperIdent                                      { }
/*  | TOBracket TCBracket                           { } */
 | TOParen TCParen                               { }
 | TColonColon                                  { }
/*  | TOParen TColonColon TCParen                    { "::" } */
 | Tfalse                                       { }
 | Ttrue                                        { }
 
/*(*----------------------------*)*/
/*(* Qualified names *)*/
/*(*----------------------------*)*/

mod_longident:
 | TUpperIdent                                      { }
 | mod_longident TDot TUpperIdent                    { }

mod_ext_longident:
 | TUpperIdent                                      { }
 | mod_ext_longident TDot TUpperIdent                { }
 | mod_ext_longident TOParen mod_ext_longident TCParen { }

type_longident:
 | TLowerIdent                                      { }
 | mod_ext_longident TDot TLowerIdent                { }

/*(*----------------------------*)*/
/*(* Misc names *)*/
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

type_declarations:
 | type_declaration                            { }
 | type_declarations TAnd type_declaration      { }

type_declaration:
  type_parameters TLowerIdent type_kind /*TODO constraints*/
      { }

type_kind:
 | /*empty*/
      { }
 | TEq core_type
      { }
 | TEq constructor_declarations
      { }
 | TEq /*TODO private_flag*/ TPipe constructor_declarations
      { }


constructor_declarations:
 | constructor_declaration                     { }
 | constructor_declarations TPipe constructor_declaration { }

constructor_declaration:
    constr_ident constructor_arguments          { }

constructor_arguments:
 | /*empty*/                                   { }
 | Tof core_type_list                           { }


type_parameters:
 |  /*empty*/                                   { }
 | type_parameter                              { }
 | TOParen type_parameter_list TCParen           { }

type_parameter_list:
 | type_parameter                              { }
 | type_parameter_list TComma type_parameter    { }

type_parameter:
    /*TODO type_variance*/ TQuote ident                   { }


/*(*----------------------------*)*/
/*(* Exceptions *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Types expressions *)*/
/*(*----------------------------*)*/

core_type:
 | core_type2
    { }

core_type2:
 | simple_core_type_or_tuple
      { }
 | core_type2 TArrow core_type2
      {  }

simple_core_type_or_tuple:
 | simple_core_type                            { }
 | simple_core_type TStar core_type_list     { }

simple_core_type:
 | simple_core_type2  %prec below_SHARP
      { }
 | TOParen core_type_comma_list TCParen %prec below_SHARP
      { }

simple_core_type2:
 | TQuote ident
      { }
 | type_longident
      { }
 | simple_core_type2 type_longident
      { }
 | TOParen core_type_comma_list TCParen type_longident
      { }


core_type_comma_list:
 | core_type                                   { }
 | core_type_comma_list TComma core_type        { }

core_type_list:
  | simple_core_type                            { }
  | core_type_list TStar simple_core_type        { }

/*(*----------------------------*)*/
/*(* Misc *)*/
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
/*(* Misc *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/


