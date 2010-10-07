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

let (qufix: long_name -> tok -> (string wrap) -> long_name) = 
 fun longname dottok ident ->
  match longname with
  | xs, Name ident2 ->
      xs ++ [Name ident2, dottok], Name ident
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
 *  - name tags (`Foo)
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

implementation: structure EOF                        { $1 ++ [FinalDef $2] }

/*(*************************************************************************)*/
/*(* Signature *)*/
/*(*************************************************************************)*/

signature:
 | /* empty */                                  { [] }
 | signature signature_item                     { $1 ++ [Item $2] }
 | signature signature_item TSemiColonSemiColon { $1 ++ [Item $2; ScSc $3] }

signature_item:
 | Ttype type_declarations
     { TypeDecl ($1, $2) }
 | Tval val_ident TColon core_type
     { ValDecl ($1, Name $2, $3, $4) }
 | Texternal val_ident TColon core_type TEq primitive_declaration
     { ExternalDecl ($1, Name $2, $3, $4, $5, $6) }
 | Texception TUpperIdent constructor_arguments
     { ExceptionDecl ($1, Name $2, $3) }

 | Topen mod_longident
     { Open ($1, $2) }

 | Tmodule Ttype ident TEq module_type
     { ItemTodo }
 | Tmodule TUpperIdent module_declaration
     { ItemTodo }



/*(*----------------------------*)*/
/*(* Misc *)*/
/*(*----------------------------*)*/

primitive_declaration:
 | TString                                      { [$1] }
 | TString primitive_declaration                { $1::$2 }

/*(*************************************************************************)*/
/*(* Structure *)*/
/*(*************************************************************************)*/

/*(* pad: should not allow those toplevel seq_expr *)*/
structure:
 | structure_tail                              { $1 }
 | seq_expr structure_tail                     { TopSeqExpr $1::$2 }

structure_tail:
 |  /* empty */                                 { [] }
 | TSemiColonSemiColon                          { [ScSc $1] }
 | TSemiColonSemiColon seq_expr structure_tail  { ScSc $1::TopSeqExpr $2::$3 }
 | TSemiColonSemiColon structure_item structure_tail  { ScSc $1::Item $2::$3 }
 | structure_item structure_tail                      { Item $1::$2 }

structure_item:
 /*(* as in signature_item *)*/
 | Ttype type_declarations
     { TypeDecl ($1, $2) }
 | Texception TUpperIdent constructor_arguments
     { ExceptionDecl ($1, Name $2, $3) }
 | Texternal val_ident TColon core_type TEq primitive_declaration
     { ExternalDecl ($1, Name $2, $3, $4, $5, $6)  }

 | Topen mod_longident
      { Open ($1, $2) }

 /*(* start of deviation *)*/
 | Tlet rec_flag let_bindings
      { Let ($1, $2, $3) }


 | Tmodule TUpperIdent module_binding
      { ItemTodo }
 | Tmodule Ttype ident TEq module_type
      { ItemTodo }
 | Tinclude module_expr
      { ItemTodo }



/*(*************************************************************************)*/
/*(* Names *)*/
/*(*************************************************************************)*/

val_ident:
 | TLowerIdent                                { $1 }
 | TOParen operator TCParen                   { ("TODOOPERATOR", $1) }

operator:
 | TPrefixOperator      { }
 | TInfixOperator       { }
 | TStar     { }
 | TEq       { }
 | Tor       { }
  /*(* but not Tand, because of conflict ? *)*/
 | TAnd      { }
 | TAssign   { }
 | Tmod      { }
 | Tland     { }
 | Tlor      { }
 | Tlxor     { }
 | Tlsl      { } 
 | Tlsr      { }
 | Tasr      { }

 | TBang     { }
 | TPlus     { }
 | TPlusDot  { }
 | TMinus    { }
 | TMinusDot { }
 | TLess     { }
 | TGreater  { }


/*(* for polymorphic types both 'a and 'A is valid. Same for module types. *)*/
ident:
 | TUpperIdent                                      { $1 }
 | TLowerIdent                                      { $1 }


constr_ident:
 | TUpperIdent     { }
 | TOParen TCParen { }
 | TColonColon     { }
 | Tfalse          { }
 | Ttrue           { }
/*  | TOBracket TCBracket                           { } */
/*  | TOParen TColonColon TCParen                    { "::" } */

/*(* record field name *)*/
label:
    TLowerIdent                                      { }


name_tag:
    TBackQuote ident                             { }

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
 | TUpperIdent                       { [], Name $1 }
 | mod_longident TDot TUpperIdent    { qufix $1 $2 $3 }

mod_ext_longident:
 | TUpperIdent                                  { [], Name $1 }
 | mod_ext_longident TDot TUpperIdent           { qufix $1 $2 $3 }
 | mod_ext_longident TOParen mod_ext_longident TCParen 
     { [], Name ("TODOEXTMODULE", $2) }



type_longident:
 | TLowerIdent                               { [], Name $1 }
 | mod_ext_longident TDot TLowerIdent        { qufix $1 $2 $3 }

val_longident:
 | val_ident                                   { [], Name $1 }
 | mod_longident TDot val_ident                { qufix $1 $2 $3 }

constr_longident:
 | mod_longident       %prec below_DOT     { }
 | TOBracket TCBracket                     { }
 | TOParen TCParen                         { }
 | Tfalse                                  { }
 | Ttrue                                   { }

/*(* record field name *)*/
label_longident:
 | TLowerIdent                              { [], Name $1 }
 | mod_longident TDot TLowerIdent           { qufix $1 $2 $3 }


class_longident:
 | TLowerIdent                               { [], Name $1 }
 | mod_longident TDot TLowerIdent            { qufix $1 $2 $3 }

mty_longident:
 | ident                                      { [], Name $1 }
 | mod_ext_longident TDot ident               { qufix $1 $2 $3 }

/*(*----------------------------*)*/
/*(* Misc names *)*/
/*(*----------------------------*)*/


/*(*************************************************************************)*/
/*(* Expressions *)*/
/*(*************************************************************************)*/

seq_expr:
 | expr        %prec below_SEMI        { [Left $1] }
 | expr TSemiColon seq_expr            { Left $1::Right $2::$3 }
 /*(* bad ? should be removed ? but it's convenient in certain contexts like
    * begin end to allow ; as a terminator
    *)*/
 | expr TSemiColon                     { [Left $1; Right $2] }


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


 | simple_expr TDot TOParen seq_expr TCParen TAssignMutable expr
      { }
 | simple_expr TDot TOBracket seq_expr TCBracket TAssignMutable expr
      { }
     

 | Tassert simple_expr %prec below_SHARP
     { }

 | name_tag simple_expr %prec below_SHARP
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
 | simple_expr TDot TOBracket seq_expr TCBracket
      { }

 /*(* object extension *)*/
 | simple_expr TSharp label
      { }
 | Tnew class_longident
      { }

 /*(* name tag extension *)*/
 | name_tag %prec prec_constant_constructor
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
 | expr_comma_list TComma expr                  { $1 ++ [Right $2; Left $3] }
 | expr TComma expr                             { [Left $1; Right $2; Left $3] }

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

 /*(* name tag extension *)*/
 | name_tag pattern %prec prec_constr_appl
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

 /*(* name tag extension *)*/
 | name_tag
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
 | pattern_comma_list TComma pattern            { $1 ++ [Right $2; Left $3] }
 | pattern TComma pattern                       { [Left $1; Right $2; Left $3] }

/*(*************************************************************************)*/
/*(* Types *)*/
/*(*************************************************************************)*/

type_constraint:
 | TColon core_type           { }
 
 /*(* object cast extension *)*/
 | TColonGreater core_type    { }

/*(*----------------------------*)*/
/*(* Types definitions *)*/
/*(*----------------------------*)*/

type_declarations:
 | type_declaration                            { [Left $1] }
 | type_declarations Tand type_declaration     { $1 ++ [Right $2; Left $3] }

type_declaration:
  type_parameters TLowerIdent type_kind /*TODO constraints*/
      { }

type_kind:
 | /*(*empty*)*/
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
 | /*(*empty*)*/                            { NoConstrArg }
 | Tof core_type_list                       { Of ($1, $2) }


type_parameters:
 |  /*(*empty*)*/                              { }
 | type_parameter                              { }
 | TOParen type_parameter_list TCParen         { }

type_parameter_list:
 | type_parameter                               { [Left $1] }
 | type_parameter_list TComma type_parameter    { $1 ++ [Right $2; Left $3] }

type_parameter:
    /*TODO type_variance*/ TQuote ident   { }



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
    { $1 }

core_type2:
 | simple_core_type_or_tuple
     { $1 }
 | core_type2 TArrow core_type2
     { TyFunction ($1, $2, $3) }
 /*(* ext: olabl *)*/
 | TLowerIdent           TColon core_type2 TArrow core_type2
     { TyTodo  }
 | TQuestion TLowerIdent TColon core_type2 TArrow core_type2
     { TyTodo }
 /*(* pad: only because of lexer hack around labels *)*/
 | TOptLabelDecl                core_type2 TArrow core_type2
     { TyTodo }


simple_core_type_or_tuple:
 | simple_core_type                          { $1 }
 | simple_core_type TStar core_type_list     { TyTuple (Left $1::Right $2::$3) }


simple_core_type:
 | simple_core_type2  %prec below_SHARP
      { $1 }
 | TOParen core_type_comma_list TCParen %prec below_SHARP
      { TyTodo }

simple_core_type2:
 | TQuote ident
      { TyVar ($1, Name $2) }
 | type_longident
      { TyName ($1) }
 | simple_core_type2 type_longident
      { TyApp (TyArg1 $1, $2) }
 | TOParen core_type_comma_list TCParen type_longident
      { TyApp (TyArgMulti (($1, $2, $3)), $4) }

 /*(* name tag extension *)*/
 | TOBracket row_field TPipe row_field_list TCBracket
      { TyTodo }
 | TOBracket tag_field TCBracket
      { TyTodo }


core_type_comma_list:
 | core_type                                  { [Left $1] }
 | core_type_comma_list TComma core_type      { $1 ++ [Right $2; Left $3] }

core_type_list:
  | simple_core_type                         { [Left $1] }
  | core_type_list TStar simple_core_type    { $1 ++ [Right $2; Left $3] }

/*(*----------------------------*)*/
/*(* Misc *)*/
/*(*----------------------------*)*/

poly_type:
 | core_type
     { }


row_field_list:
 | row_field                                   { }
 | row_field_list TPipe row_field                { }

row_field:
 | tag_field                                   { }
 | simple_core_type2                           { }

tag_field:
 | name_tag Tof opt_ampersand amper_type_list
      { }
 | name_tag
      { }

opt_ampersand:
 | TAnd                                   { }
 | /* empty */                                 { }

amper_type_list:
 | core_type                                   { }
 | amper_type_list TAnd core_type         { }



/*(*************************************************************************)*/
/*(* Let, definitions *)*/
/*(*************************************************************************)*/

let_bindings:
 | let_binding                           { [Left $1] }
 | let_bindings Tand let_binding         { $1 ++ [Right $2; Left $3] }


let_binding:
 | val_ident fun_binding
      { 
        let (args, (teq, body)) = $2 in
        LetClassic {
          l_name = Name $1;
          l_args = args;
          l_tok = teq;
          l_body = body;
        }
      }
 | pattern TEq seq_expr
      { LetPattern ($1, $2, $3) }



fun_binding:
 | strict_binding { $1 }

strict_binding:
 /*(* simple values, e.g. 'let x = 1' *)*/
 | TEq seq_expr
      { [], ($1, $2) }
 /*(* function values, e.g. 'let x a b c = 1' *)*/
 | labeled_simple_pattern fun_binding
      { let (args, body) = $2 in $1::args, body }


labeled_simple_pattern:
  | simple_pattern
      { }
  | label_pattern
      { }


rec_flag:
 | /*(*empty*)*/   { None }
 | Trec            { Some $1 }


label_let_pattern:
 | label_var
      { }
 | label_var TColon core_type
      { }

opt_default:
 | /*(*empty*)*/           { None  }
 | TEq seq_expr            { Some ($1, $2) }

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

module_declaration:
 | TColon module_type
      { }

/*(*----------------------------*)*/
/*(* Module types *)*/
/*(*----------------------------*)*/

module_type:
 | mty_longident
      { }
 | Tsig signature Tend
      { }
 | Tfunctor TOParen TUpperIdent TColon module_type TCParen TArrow module_type
      %prec below_WITH
      { }

/*(*----------------------------*)*/
/*(* Module expressions *)*/
/*(*----------------------------*)*/

module_expr:
  /*(* when just do a module aliasing *)*/
  | mod_longident
      { }
  /*(* nested modules *)*/
  | Tstruct structure Tend
      { }
  /*(* functor definition *)*/
  | Tfunctor TOParen TUpperIdent TColon module_type TCParen TArrow module_expr
      { }
  /*(* module/functor application *)*/
  | module_expr TOParen module_expr TCParen
      { }

/*(*************************************************************************)*/
/*(* Misc *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(* xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

opt_semi:
 | /*(*empty*)*/    { }
 | TSemiColon       { }

opt_bar:
 | /*(*empty*)*/    { }
 | TPipe            { }
