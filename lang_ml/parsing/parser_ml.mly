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
 Tand 
 Tor Tmod Tlor Tlsl Tlsr Tlxor Tasr Tland

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

%token <Ast_ml.info> TMinusDot TPlusDot

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
%nonassoc Tand             /* above Twith (module rec A: Tsig with ... and ...) */
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
%right    TAnd TAndAnd                   /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 TEq TLess TGreater    /* expr (e OP e OP e) */
%right    INFIXOP1                       /* expr (e OP e OP e) */
%right    TColonColon                    /* expr (e :: e :: e) */
%left     INFIXOP2 TPlus TPlusDot TMinus TMinusDot  /* expr (e OP e OP e) */
%left     INFIXOP3 TStar                 /* expr (e OP e OP e) */
%left     TInfixOperator /* pad: */
%right    INFIXOP4                       /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor      /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl               /* above Tas TPipe TColonColon TComma */
%nonassoc below_SHARP
%nonassoc TSharp                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc TDot
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc TBackQuote TBang Tbegin TChar Tfalse TFloat TInt
          TOBrace TOBraceLess TOBracket TOBracketPipe TLowerIdent TOParen
          Tnew TPrefixOperator TString Ttrue TUpperIdent

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start interface  implementation
%type <Ast_ml.toplevel list> interface
%type <Ast_ml.toplevel list> implementation

%%

/*(*************************************************************************)*/
/*(* TOC *)*/
/*(*************************************************************************)*/
/*
(*
 * the value constructions:
 *  - constants
 *  - constructors
 *  - lists
 *  - records
 *  - tuples 
 *  - arrays
 *  - symbols (`Foo)
 * have at the same time some rules to express:
 *  - the type
 *  - the expression
 *  - the pattern
 *)
 */

/*(*************************************************************************)*/
/*(* Toplevel, compilation units *)*/
/*(*************************************************************************)*/

interface:      signature EOF                        { [FinalDef $2] }

implementation: structure EOF                        { [FinalDef $2] }

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
 | Texception TUpperIdent constructor_arguments
      { }

 | Topen mod_longident
      { }

/*(*----------------------------*)*/
/*(* Misc *)*/
/*(*----------------------------*)*/

primitive_declaration:
 | TString                                      { }
 | TString primitive_declaration                { }

/*(*************************************************************************)*/
/*(* Structure *)*/
/*(*************************************************************************)*/

/*(* pad: should not allow those toplevel seq_expr *)*/
structure:
 | structure_tail                              { }
 | seq_expr structure_tail                     { }

structure_tail:
 |  /* empty */                                 { }
 | TSemiColonSemiColon                                    { }
 | TSemiColonSemiColon seq_expr structure_tail            { }
 | TSemiColonSemiColon structure_item structure_tail      { }
 | structure_item structure_tail               { }

structure_item:
 /*(* as in signature_item *)*/
 | Texternal val_ident TColon core_type TEq primitive_declaration
     { }
 | Ttype type_declarations
     { }
 | Texception TUpperIdent constructor_arguments
     { }

 | Topen mod_longident
      { }

 /*(* start of deviation *)*/

 | Tlet rec_flag let_bindings
      {  }


 | Tmodule TUpperIdent module_binding
      { }
 | Tinclude module_expr
      { }



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
 | TAnd                                   { }
 | TAssign                                  { }
 | Tmod { }
 | Tland { }
 | Tlor { }
 | Tlxor { }
 | Tlsl { } 
 | Tlsr { }
 | Tasr { }

 | TBang                                        { }
 | TPlus                                        { }
 | TPlusDot                                     { }
 | TMinus                                       { }
 | TMinusDot                                    { }
 | TLess                                        { }
 | TGreater                                     { }


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

/*(* record field name *)*/
label:
    TLowerIdent                                      { }

/*(*----------------------------*)*/
/*(* Labels *)*/
/*(*----------------------------*)*/

label_var:
    TLowerIdent    { }

label_ident:
    TLowerIdent   { }

 
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

val_longident:
 | val_ident                                   { }
 | mod_longident TDot val_ident                 { }

constr_longident:
 | mod_longident       %prec below_DOT         { }
 | TOBracket TCBracket                           { }
 | TOParen TCParen                               { }
 | Tfalse                                       { }
 | Ttrue                                        { }

/*(* record field name *)*/
label_longident:
 | TLowerIdent                                      { }
 | mod_longident TDot TLowerIdent                    { }

/*(*----------------------------*)*/
/*(* Misc names *)*/
/*(*----------------------------*)*/


/*(*************************************************************************)*/
/*(* Expressions *)*/
/*(*************************************************************************)*/

seq_expr:
 | expr        %prec below_SEMI  { }
 | expr TSemiColon                     { }
 | expr TSemiColon seq_expr            { }


expr:
 | simple_expr %prec below_SHARP
      { }
 /*(* function application *)*/
 | simple_expr simple_labeled_expr_list
      { }

 | Tlet rec_flag let_bindings Tin seq_expr
      { }
 | Tfun labeled_simple_pattern fun_def
      { }
 | Tfunction opt_bar match_cases
     { }

 | expr_comma_list %prec below_COMMA
     { }
 | constr_longident simple_expr %prec below_SHARP
     { }
 | expr TColonColon expr
     { }

 | expr TInfixOperator expr
      { }

 | Tif seq_expr Tthen expr Telse expr
     { }
 | Tif seq_expr Tthen expr
      { }

 | Tmatch seq_expr Twith opt_bar match_cases
      { }

 | Ttry seq_expr Twith opt_bar match_cases
      { }

 | Twhile seq_expr Tdo seq_expr Tdone
     { }
 | Tfor val_ident TEq seq_expr direction_flag seq_expr Tdo seq_expr Tdone
     { }

 | expr TAssign expr
      { }

 | expr TEq expr
      { }


 | expr TPlus expr
      { }
 | expr TMinus expr
     { }
 | expr TPlusDot expr
     { }
 | expr TMinusDot expr
     { }
 | expr TStar expr
     { }
 | expr TLess expr
     { }
 | expr TGreater expr
     { }
 | expr Tor expr
     { }
 | expr TAnd expr
     { }
 | expr TAndAnd expr
     { }
 | subtractive expr %prec prec_unary_minus
     { }
 | additive expr %prec prec_unary_plus
     { }

 | simple_expr TDot label_longident TAssignMutable expr
      { }
     

 | Tassert simple_expr %prec below_SHARP
     { }



simple_expr:
 | constant
      { }
 | val_longident
      { }
 /*(* this includes 'false' *)*/
 | constr_longident %prec prec_constant_constructor
      { }
 | simple_expr TDot label_longident
      { }

 | TOParen seq_expr TCParen
      { }
 | Tbegin seq_expr Tend
     { }
 | Tbegin Tend
     { }

 /*(* bugfix: must be in simple_expr. Originally made the mistake to put it
    * in expr: and the parser would then not recognize things like 'foo !x'
    *)*/
 | TPrefixOperator simple_expr
      { }
 | TBang simple_expr
     { }


 | TOBrace record_expr TCBrace
      { }

 | TOBracket expr_semi_list opt_semi TCBracket
      { }
 | TOBracketPipe expr_semi_list opt_semi TPipeCBracket
      { }
 | TOBracketPipe TPipeCBracket
      { }

 /*(* array extension *)*/
 | simple_expr TDot TOParen seq_expr TCParen
      { }

 /*(* object extension *)*/
 | simple_expr TSharp label
      { }

 | TOParen seq_expr type_constraint TCParen
      { }




simple_labeled_expr_list:
 | labeled_simple_expr
      { }
 | simple_labeled_expr_list labeled_simple_expr
      { }

labeled_simple_expr:
 | simple_expr %prec below_SHARP
      { }
 | label_expr
      { }


expr_comma_list:
 | expr_comma_list TComma expr                  { }
 | expr TComma expr                             { }

expr_semi_list:
 | expr                                        { }
 | expr_semi_list TSemiColon expr                    { }





record_expr:
 | simple_expr Twith lbl_expr_list opt_semi     { }
 | lbl_expr_list opt_semi                      { }

lbl_expr_list:
 | label_longident TEq expr
      { }
 | label_longident
      { }
 | lbl_expr_list TSemiColon label_longident TEq expr
     { }
 | lbl_expr_list TSemiColon label_longident
     { }


subtractive:
  | TMinus                                       { }
  | TMinusDot                                    { }

additive:
  | TPlus                                        { }
  | TPlusDot                                     { }


direction_flag:
 | Tto                                          { }
 | Tdownto                                      { }

/*(*----------------------------*)*/
/*(* Labels *)*/
/*(*----------------------------*)*/

label_expr:
 | TLabelDecl simple_expr %prec below_SHARP
      { }
 | TTilde label_ident
      { }
 | TQuestion label_ident
      { }
 | TOptLabelDecl simple_expr %prec below_SHARP
      { }

/*(*----------------------------*)*/
/*(* Constants *)*/
/*(*----------------------------*)*/

constant:
 | TInt                                         { }
 | TChar                                        { }
 | TString                                      { }
 | TFloat                                       { }

signed_constant:
 | constant                                    { }
 | TMinus TInt                                   { }
 | TMinus TFloat                                 { }
 | TPlus TInt                                    { }
 | TPlus TFloat                                  { }

/*(*************************************************************************)*/
/*(* Patterns *)*/
/*(*************************************************************************)*/

match_cases:
 | pattern match_action                        { }
 | match_cases TPipe pattern match_action        { }

match_action:
 | TArrow seq_expr                       { }
 | Twhen seq_expr TArrow seq_expr         { }




pattern:
 | simple_pattern
      { }
 | constr_longident pattern %prec prec_constr_appl
      { }

 | pattern_comma_list  %prec below_COMMA
      { }
 | pattern TColonColon pattern
      { }

 | pattern Tas val_ident
      { }

 /*(* nested patterns *)*/
 | pattern TPipe pattern
      { }


simple_pattern:
 | val_ident %prec below_EQUAL
      { }
 | constr_longident
      { }
 | TUnderscore
      { }
 | signed_constant
      { }

 | TOBrace lbl_pattern_list record_pattern_end TCBrace
      { }

 | TOBracket pattern_semi_list opt_semi TCBracket
      { }
 | TOBracketPipe pattern_semi_list opt_semi TPipeCBracket
      { }
 | TOBracketPipe TPipeCBracket
      { }

 /*(* note that let (x:...) a =  will trigger this rule *)*/
 | TOParen pattern TColon core_type TCParen
      { }

 | TOParen pattern TCParen
      { }


lbl_pattern_list:
 | label_longident TEq pattern               { }
 | label_longident                             { }
 | lbl_pattern_list TSemiColon label_longident TEq pattern { }
 | lbl_pattern_list TSemiColon label_longident       { }

record_pattern_end:
 |  opt_semi                                    { }
 /*(* new 3.12 feature! *)*/
 | TSemiColon TUnderscore opt_semi                    { }


pattern_semi_list:
 | pattern                                     { }
 | pattern_semi_list TSemiColon pattern              { }

pattern_comma_list:
 | pattern_comma_list TComma pattern            { }
 | pattern TComma pattern                       { }

/*(*************************************************************************)*/
/*(* Types *)*/
/*(*************************************************************************)*/

type_constraint:
 | TColon core_type                             { }

/*(*----------------------------*)*/
/*(* Types definitions *)*/
/*(*----------------------------*)*/

type_declarations:
 | type_declaration                            { }
 | type_declarations Tand type_declaration     { }

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
 | TEq /*TODO private_flag*/ TOBrace label_declarations opt_semi TCBrace
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



label_declarations:
 | label_declaration                           { }
 | label_declarations TSemiColon label_declaration   { }

label_declaration:
    mutable_flag label TColon poly_type          { }

mutable_flag:
 | /* empty */                                 { }
 | Tmutable                                     { }


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
 /*(* ext: olabl *)*/
 | TLowerIdent           TColon core_type2 TArrow core_type2
      {  }
 | TQuestion TLowerIdent TColon core_type2 TArrow core_type2
      { }
 /*(* pad: only because of lexer hack around labels *)*/
 | TOptLabelDecl                core_type2 TArrow core_type2
     { }


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

poly_type:
 | core_type
     { }

/*(*************************************************************************)*/
/*(* Let, definitions *)*/
/*(*************************************************************************)*/

let_bindings:
 | let_binding                                 { }
 | let_bindings Tand let_binding                { }


let_binding:
 | val_ident fun_binding
      { }
 | pattern TEq seq_expr
      { }



fun_binding:
 | strict_binding
      { }

strict_binding:
 /*(* simple values, e.g. 'let x = 1' *)*/
 | TEq seq_expr
      { }
 /*(* function values, e.g. 'let x a b c = 1' *)*/
 | labeled_simple_pattern fun_binding
      { }


labeled_simple_pattern:
  | simple_pattern
      { }
  | label_pattern
      { }


rec_flag:
 | /* empty */                                 { }
 | Trec                                         { }


label_let_pattern:
 | label_var
      { }
 | label_var TColon core_type
      { }

opt_default:
 | /* empty */                         { }
 | TEq seq_expr                      { }

/*(*----------------------------*)*/
/*(* Labels *)*/
/*(*----------------------------*)*/

label_pattern:
  | TTilde label_var
      { }
  /*(* ex: let x ~foo:a *)*/
  | TLabelDecl simple_pattern
      { }
  | TTilde TOParen label_let_pattern TCParen
      { }
  | TQuestion TOParen label_let_pattern opt_default TCParen
      { }
  | TQuestion label_var
      { }

/*(*************************************************************************)*/
/*(* Fun, definitions *)*/
/*(*************************************************************************)*/

fun_def:
 | match_action                                { }
 | labeled_simple_pattern fun_def
      { }


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

module_binding:
 | TEq module_expr
      { }

/*(*----------------------------*)*/
/*(* Module types *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(* Module expressions *)*/
/*(*----------------------------*)*/

module_expr:
  /*(* when just do a module aliasing *)*/
  | mod_longident
      { }

  | Tstruct structure Tend
      { }

/*(*************************************************************************)*/
/*(* Misc *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

opt_semi:
 | /* empty */                                 { }
 | TSemiColon                                        { }

opt_bar:
 | /* empty */                                 { }
 | TPipe                                         { }
