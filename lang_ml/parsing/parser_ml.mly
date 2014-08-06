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
 * src: adapted from the official source of OCaml in its
 * parsing/ subdirectory. All semantic actions are new. Only the
 * grammar structure was copied.
 * 
 * was: $Id: parser.mly 10536 2010-06-07 15:32:32Z doligez $
 *
 * other sources:
 * - http://caml.inria.fr/pub/docs/manual-ocaml/language.html
 *  (note that it unfortunately contains conflicts when translated into yacc).
 * - http://www.cs.ru.nl/~tews/htmlman-3.10/full-grammar.html
 *   itself derived from the official ocaml reference manual
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/sml.html
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/hamlet/
 *   solves ambiguities
 * - linear-ML parser
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
      xs @ [Name ident2, dottok], Name ident

let to_item xs =
  xs +> Common.map_filter (function
  | TopItem x -> Some x
  | _ -> None
  )
%}
/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(* unrecognized token, will generate parse error *)*/
%token <Parse_info.info> TUnknown

%token <Parse_info.info> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Parse_info.info> TCommentSpace TCommentNewline   TComment
%token <Parse_info.info> TCommentMisc

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Parse_info.info> TInt TFloat TChar TString
%token <string * Parse_info.info> TLowerIdent TUpperIdent
%token <string * Parse_info.info> TLabelUse TLabelDecl TOptLabelUse TOptLabelDecl

/*(* keywords tokens *)*/
%token <Parse_info.info>
 Tfun Tfunction Trec Ttype Tof Tif Tthen Telse
 Tmatch Twith Twhen
 Tlet Tin Tas
 Ttry Texception
 Tbegin Tend Tfor Tdo Tdone Tdownto Twhile Tto
 Tval Texternal
 Ttrue Tfalse
 Tmodule Topen Tfunctor Tinclude Tsig Tstruct
 Tclass Tnew Tinherit Tconstraint Tinitializer Tmethod Tobject Tprivate
 Tvirtual
 Tlazy Tmutable Tassert
 Tand 
 Tor Tmod Tlor Tlsl Tlsr Tlxor Tasr Tland

/*(* syntax *)*/
%token <Parse_info.info> 
 TOParen TCParen TOBrace TCBrace TOBracket TCBracket
 TOBracketPipe TPipeCBracket  TOBracketLess TGreaterCBracket
 TOBraceLess TGreaterCBrace
 TOBracketGreater TColonGreater
 TDot TDotDot 
 TComma TEq TAssign TAssignMutable TColon TColonColon
 TBang TBangEq TTilde TPipe
 TSemiColon TSemiColonSemiColon
 TQuestion TQuestionQuestion
 TUnderscore TStar TArrow TQuote TBackQuote TAnd TAndAnd TSharp
 TMinusDot TPlusDot

/*(* operators *)*/
%token <Parse_info.info> TPlus TMinus TLess TGreater
%token <string * Parse_info.info> TPrefixOperator TInfixOperator

/*(*-----------------------------------------*)*/
/*(*2 extra tokens: *)*/
/*(*-----------------------------------------*)*/
%token <Parse_info.info> TSharpDirective

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
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
%left     TBangEq
%right    INFIXOP1                       /* expr (e OP e OP e) */
%right    TColonColon                    /* expr (e :: e :: e) */
%left     INFIXOP2 TPlus TPlusDot TMinus TMinusDot  /* expr (e OP e OP e) */
%left     INFIXOP3 TStar                 /* expr (e OP e OP e) */
%left     TInfixOperator /* pad: */
%right    INFIXOP4                       /* expr (e OP e OP e) */
%left     Tmod Tlor Tlxor Tland
%right    Tlsr Tasr Tlsl

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
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%start interface  implementation
%type <Ast_ml.toplevel list> interface
%type <Ast_ml.toplevel list> implementation

%%

/*(*************************************************************************)*/
/*(*1 TOC *)*/
/*(*************************************************************************)*/
/*
(*
 * - toplevel
 * - signature
 * - structure
 * - names
 * 
 * - expression
 * - type
 * - pattern
 * with for the last 3 sections subsections around values:
 *    - constants
 *    - constructors
 *    - lists
 *    - records
 *    - tuples 
 *    - arrays
 *    - name tags (`Foo)
 * 
 * - let/fun
 * - classes (not in AST)
 * - modules
 * - xxx_opt, xxx_list
 * 
 *)*/
/*(*************************************************************************)*/
/*(*1 Toplevel, compilation units *)*/
/*(*************************************************************************)*/

interface:      signature EOF                        { $1 }

implementation: structure EOF                        { $1 }

/*(*************************************************************************)*/
/*(*1 Signature *)*/
/*(*************************************************************************)*/

signature:
 | /* empty */                                  { [] }
 | signature signature_item                     { $1 @ [TopItem $2] }
 | signature signature_item TSemiColonSemiColon { $1 @ [TopItem $2; ScSc $3] }

signature_item:
 | Ttype type_declarations
     { Type ($1, $2) }
 | Tval val_ident TColon core_type
     { Val ($1, Name $2, $3, $4) }
 | Texternal val_ident TColon core_type TEq primitive_declaration
     { External ($1, Name $2, $3, $4, $5, $6) }
 | Texception TUpperIdent constructor_arguments
     { Exception ($1, Name $2, $3) }

 /*(* modules *)*/
 | Topen mod_longident
     { Open ($1, $2) }

 | Tmodule Ttype ident TEq module_type
     { ItemTodo $1 }
 | Tmodule TUpperIdent module_declaration
     { ItemTodo $1 }

 /*(* objects *)*/
 | Tclass class_descriptions
      { ItemTodo $1 }

/*(*----------------------------*)*/
/*(*2 Misc *)*/
/*(*----------------------------*)*/

primitive_declaration:
 | TString                                      { [$1] }
 | TString primitive_declaration                { $1::$2 }

/*(*************************************************************************)*/
/*(*1 Structure *)*/
/*(*************************************************************************)*/

/*(* pad: should not allow those toplevel seq_expr *)*/
structure:
 | structure_tail                              { $1 }
 | seq_expr structure_tail                     { TopSeqExpr $1::$2 }

structure_tail:
 |  /* empty */                                 
     { [] }
 | TSemiColonSemiColon                          
     { [ScSc $1] }
 | TSemiColonSemiColon seq_expr structure_tail  
     { ScSc $1::TopSeqExpr $2::$3 }
 | TSemiColonSemiColon structure_item structure_tail  
     { ScSc $1::TopItem $2::$3 }
 | TSemiColonSemiColon TSharpDirective  structure_tail  
     { ScSc $1::TopDirective $2::$3 }

 | structure_item structure_tail                      
     { TopItem $1::$2 }
 | TSharpDirective structure_tail 
     { TopDirective $1::$2 }

structure_item:
 /*(* as in signature_item *)*/
 | Ttype type_declarations
     { Type ($1, $2) }
 | Texception TUpperIdent constructor_arguments
     { Exception ($1, Name $2, $3) }
 | Texternal val_ident TColon core_type TEq primitive_declaration
     { External ($1, Name $2, $3, $4, $5, $6)  }

 | Topen mod_longident
      { Open ($1, $2) }

 /*(* start of deviation *)*/
 | Tlet rec_flag let_bindings
      { Let ($1, $2, $3) }


 /*(* modules *)*/
 | Tmodule TUpperIdent module_binding
      { 
        match $3 with
        | None -> ItemTodo $1
        | Some (x, y) ->
            Module ($1, Name $2, x, y) 
      }
 | Tmodule Ttype ident TEq module_type
      { ItemTodo $1 }
 | Tinclude module_expr
      { ItemTodo $1 }

 /*(* objects *)*/
  | Tclass class_declarations
      { ItemTodo $1 }
  | Tclass Ttype class_type_declarations
      { ItemTodo $1 }

/*(*************************************************************************)*/
/*(*1 Names *)*/
/*(*************************************************************************)*/

val_ident:
 | TLowerIdent                                { $1 }
 | TOParen operator TCParen                   { ("TODOOPERATOR", $1) }

operator:
 | TPrefixOperator      { } | TInfixOperator       { }
 | TStar     { } | TEq       { } | TAssign   { } | TBang     { }
  /*(* but not Tand, because of conflict ? *)*/
 | Tor       { } | TAnd      { }
 | Tmod      { } | Tland     { } | Tlor      { } | Tlxor     { }
 | Tlsl      { } | Tlsr      { } | Tasr      { }
 | TPlus     { } | TPlusDot  { } | TMinus    { } | TMinusDot { }
 | TLess     { } | TGreater  { }
 | TAndAnd { } | TBangEq { }

/*(* for polymorphic types both 'a and 'A is valid. Same for module types. *)*/
ident:
 | TUpperIdent                                      { $1 }
 | TLowerIdent                                      { $1 }


constr_ident:
 | TUpperIdent     { $1 }
 | TOParen TCParen { "()TODO", $1 }
 | TColonColon     { "::", $1 }
 | Tfalse          { "false", $1 }
 | Ttrue           { "true", $1 }
/*  | TOBracket TCBracket                           { } */
/*  | TOParen TColonColon TCParen                    { "::" } */

/*(* record field name *)*/
label: TLowerIdent  { $1 }

name_tag: TBackQuote ident   { }

/*(*----------------------------*)*/
/*(*2 Labels *)*/
/*(*----------------------------*)*/

label_var:
    TLowerIdent    { }

/*(* for label arguments like ~x or ?x *)*/
label_ident:
    TLowerIdent   { $1 }

 
/*(*----------------------------*)*/
/*(*2 Qualified names *)*/
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
 | mod_longident       %prec below_DOT     { $1 }
 | TOBracket TCBracket                     { [], Name ("[]TODO", $1) }
 | TOParen TCParen                         { [], Name ("()TODO", $1) }
 | Tfalse                                  { [], Name ("false", $1) }
 | Ttrue                                   { [], Name ("true", $1) }

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

/*(* it's mod_ext_longident, not mod_longident *)*/
clty_longident:
 | TLowerIdent                               { [], Name $1 }
 | mod_ext_longident TDot TLowerIdent            { qufix $1 $2 $3 }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

seq_expr:
 | expr             %prec below_SEMI   { [Left $1] }
 | expr TSemiColon seq_expr            { Left $1::Right $2::$3 }

 /*(* bad ? should be removed ? but it's convenient in certain contexts like
    * begin end to allow ; as a terminator
    *)*/
 | expr TSemiColon                     { [Left $1; Right $2] }


expr:
 | simple_expr %prec below_SHARP
     { $1 }
 /*(* function application *)*/
 | simple_expr simple_labeled_expr_list
     { match $1 with
       | L name -> FunCallSimple (name, $2)
       | _      -> FunCall ($1, $2)
     }

 | Tlet rec_flag let_bindings Tin seq_expr
     { LetIn ($1, $2, $3, $4, $5) }

 | Tfun labeled_simple_pattern fun_def
     { let (params, action) = $3 in
       Fun ($1, $2::params, action)
     }

 | Tfunction opt_bar match_cases
     { Function ($1, $2 @ $3) }

 | expr_comma_list %prec below_COMMA
     { Tuple $1 }
 | constr_longident simple_expr %prec below_SHARP
     { Constr ($1, Some $2) }

 | expr TColonColon expr
     { Infix ($1, ("::", $2), $3) (* TODO ? ConsList ? *) }

 | expr TInfixOperator expr
     { Infix ($1, $2, $3) }

 | expr Tmod expr 
     { Infix ($1, ("mod", $2), $3) }
 | expr Tland expr 
     { Infix ($1, ("land", $2), $3) }
 | expr Tlor expr 
     { Infix ($1, ("lor", $2), $3) }
 | expr Tlxor expr 
     { Infix ($1, ("lxor", $2), $3) }

 | expr Tlsl expr 
     { Infix ($1, ("lsl", $2), $3) }
 | expr Tlsr expr 
     { Infix ($1, ("lsr", $2), $3) }
 | expr Tasr expr 
     { Infix ($1, ("asr", $2), $3) }

 | expr TBangEq expr
     { Infix ($1, ("!=", $2), $3) }

/*
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
*/

 | Tif seq_expr Tthen expr Telse expr
     { If ($1, $2, $3, $4, Some ($5, $6)) }
 | Tif seq_expr Tthen expr
     { If ($1, $2, $3, $4, None) }

 | Tmatch seq_expr Twith opt_bar match_cases
     { Match ($1, $2, $3, $4 @ $5) }

 | Ttry seq_expr Twith opt_bar match_cases
     { Try ($1, $2, $3, $4 @ $5) }

 | Twhile seq_expr Tdo seq_expr Tdone
     { While ($1, $2, $3, $4, $5) }
 | Tfor val_ident TEq seq_expr direction_flag seq_expr Tdo seq_expr Tdone
     { For ($1, Name $2, $3, $4, $5, $6, $7, $8, $9)  }

 | expr TAssign expr { RefAssign ($1, $2, $3) }

 | expr TEq expr   { Infix ($1, ("=", $2), $3) }

 | expr TPlus expr     { Infix ($1, ("+", $2), $3)  }
 | expr TMinus expr    { Infix ($1, ("-", $2), $3) }
 | expr TPlusDot expr  { Infix ($1, ("+.", $2), $3) }
 | expr TMinusDot expr { Infix ($1, ("-.", $2), $3) }
 | expr TStar expr     { Infix ($1, ("*", $2), $3) }
 | expr TLess expr     { Infix ($1, ("<", $2), $3) }
 | expr TGreater expr  { Infix ($1, (">", $2), $3) }
 | expr Tor expr       { Infix ($1, ("or", $2), $3) }
 | expr TAnd expr      { Infix ($1, ("&", $2), $3) }
 | expr TAndAnd expr   { Infix ($1, ("&&", $2), $3) }

 | subtractive expr %prec prec_unary_minus
     { Prefix ($1, $2) }
 | additive expr %prec prec_unary_plus
     { Prefix ($1, $2) }

 | simple_expr TDot label_longident TAssignMutable expr
      { FieldAssign ($1, $2, $3, $4, $5) }


 /*(* array extension *)*/
 | simple_expr TDot TOParen seq_expr TCParen TAssignMutable expr
     { ExprTodo }
 | simple_expr TDot TOBracket seq_expr TCBracket TAssignMutable expr
     { ExprTodo }
 /*(* bigarray extension, a.{i} <- v *)*/
 | simple_expr TDot TOBrace expr TCBrace TAssignMutable expr
     { ExprTodo }
     

 | Tlet Topen mod_longident Tin seq_expr
      { ExprTodo }

 | Tassert simple_expr %prec below_SHARP
     { ExprTodo }

 | name_tag simple_expr %prec below_SHARP
     { ExprTodo }

 | Tlazy simple_expr %prec below_SHARP
     { ExprTodo }

  /*(* objects *)*/
  | label TAssignMutable expr
      { ExprTodo }


simple_expr:
 | constant
     { C $1 }
 | val_longident
     { L $1 }
 /*(* this includes 'false' *)*/
 | constr_longident %prec prec_constant_constructor
     { Constr ($1, None) }

 | simple_expr TDot label_longident
     { FieldAccess ($1, $2, $3) }

 /*(* if only one expr then prefer to generate a ParenExpr *)*/
 | TOParen seq_expr TCParen
     { match $2 with
     | [] -> Sequence ($1, $2, $3) 
     | [Left x] -> ParenExpr ($1, x, $3)
     | [Right _] -> raise Impossible
     | _ -> Sequence ($1, $2, $3) 
     }

 | Tbegin seq_expr Tend
     { Sequence ($1, $2, $3)  }
 | Tbegin Tend
     { Sequence ($1, [], $2) }

 /*(* bugfix: must be in simple_expr. Originally made the mistake to put it
    * in expr: and the parser would then not recognize things like 'foo !x'
    *)*/
 | TPrefixOperator simple_expr
     { Prefix ($1, $2) }
 | TBang simple_expr
     { RefAccess ($1, $2) }


 | TOBrace record_expr TCBrace
     { Record ($1, $2, $3) }

 | TOBracket expr_semi_list opt_semi3 TCBracket
     { List ($1, $2 @ $3, $4) }

 | TOBracketPipe expr_semi_list opt_semi TPipeCBracket
     { ExprTodo }
 | TOBracketPipe TPipeCBracket
     { ExprTodo }

 /*(* array extension *)*/
 | simple_expr TDot TOParen seq_expr TCParen
     { ExprTodo }
 | simple_expr TDot TOBracket seq_expr TCBracket
     { ExprTodo }
 /*(* bigarray extension *)*/
 | simple_expr TDot TOBrace expr TCBrace
     { ExprTodo }

 /*(* object extension *)*/
 | simple_expr TSharp label
     { ObjAccess ($1, $2, Name $3) }
 | Tnew class_longident
     { New ($1, $2) }

 | TOBraceLess field_expr_list opt_semi TGreaterCBrace
      { ExprTodo }


 /*(* name tag extension *)*/
 | name_tag %prec prec_constant_constructor
     { ExprTodo }

 | TOParen seq_expr type_constraint TCParen
     { ExprTodo }

 /*(* scoped open, 3.12 *)*/
 | mod_longident TDot TOParen seq_expr TCParen
     { ExprTodo }

simple_labeled_expr_list:
 | labeled_simple_expr
      { [$1] }
 | simple_labeled_expr_list labeled_simple_expr
      { $1 @ [$2] }

labeled_simple_expr:
 | simple_expr %prec below_SHARP
      { ArgExpr $1 }
 | label_expr
      { $1 }


expr_comma_list:
 | expr_comma_list TComma expr                  { $1 @ [Right $2; Left $3] }
 | expr TComma expr                             { [Left $1; Right $2; Left $3] }

expr_semi_list:
 | expr                                  { [Left $1] }
 | expr_semi_list TSemiColon expr        { $1 @ [Right $2; Left $3] }





record_expr:
 | lbl_expr_list opt_semi                    { RecordNormal ($1 @ $2) }
 | simple_expr Twith lbl_expr_list opt_semi  { RecordWith ($1, $2, $3 @ $4) }

lbl_expr_list:
 | label_longident TEq expr
     { [Left (FieldExpr ($1, $2, $3))] }
 | lbl_expr_list TSemiColon     label_longident TEq expr
     { $1 @ [Right $2; Left (FieldExpr ($3, $4, $5))] }
 /*(* new 3.12 feature! *)*/
 | label_longident
      { [Left (FieldImplicitExpr ($1))] }
 | lbl_expr_list TSemiColon     label_longident
     { $1 @ [Right $2; Left (FieldImplicitExpr $3)] }


subtractive:
  | TMinus                                       { "-", $1 }
  | TMinusDot                                    { "-.", $1 }

additive:
  | TPlus                                        { "+", $1 }
  | TPlusDot                                     { "+.", $1 }


direction_flag:
 | Tto                                          { To $1 }
 | Tdownto                                      { Downto $1 }


/*(*----------------------------*)*/
/*(*2 Constants *)*/
/*(*----------------------------*)*/

constant:
 | TInt     { Int $1 }
 | TChar    { Char $1 }
 | TString  { String $1 }
 | TFloat   { Float $1 }

/*(*----------------------------*)*/
/*(*2 Labels *)*/
/*(*----------------------------*)*/

label_expr:
 | TTilde label_ident
      { ArgImplicitTildeExpr ($1, Name $2) }
 | TQuestion label_ident
      { ArgImplicitQuestionExpr ($1, Name $2) }
 | TLabelDecl simple_expr %prec below_SHARP
      { ArgLabelTilde (Name $1 (* TODO remove the ~ and : *), $2) }
 | TOptLabelDecl simple_expr %prec below_SHARP
      { ArgLabelQuestion (Name $1 (* TODO remove the ~ and : *), $2) }

/*(*----------------------------*)*/
/*(*3 objects *)*/
/*(*----------------------------*)*/

field_expr_list:
 |  label TEq expr
      { }
  | field_expr_list TSemiColon label TEq expr
      { }


/*(*************************************************************************)*/
/*(*1 Patterns *)*/
/*(*************************************************************************)*/

match_cases:
 | pattern  match_action                     { [Left ($1, $2)] }
 | match_cases TPipe    pattern match_action { $1 @ [Right $2; Left ($3, $4)] }

match_action:
 | TArrow seq_expr                  { Action ($1, $2) }
 | Twhen seq_expr TArrow seq_expr   { WhenAction ($1, $2, $3, $4) }


pattern:
 | simple_pattern
      { $1 }

 | constr_longident pattern %prec prec_constr_appl
      { PatConstr ($1, Some $2) }
 | pattern_comma_list  %prec below_COMMA
      { PatTuple ($1) }
 | pattern TColonColon pattern
      { PatConsInfix ($1, $2, $3) }

 | pattern Tas val_ident
      { PatAs ($1, $2, Name $3) }

 /*(* nested patterns *)*/
 | pattern TPipe pattern
      { PatDisj ($1, $2, $3) }

 /*(* name tag extension *)*/
 | name_tag pattern %prec prec_constr_appl
      { PatTodo }




simple_pattern:
 | val_ident %prec below_EQUAL
      { PatVar (Name $1) }
 | constr_longident
      { PatConstr ($1, None) }
 | TUnderscore
      { PatUnderscore $1 }
 | signed_constant
      { PatConstant $1 }

 | TOBrace lbl_pattern_list record_pattern_end TCBrace
      { PatRecord ($1, $2, (* $3 *) $4) }
 | TOBracket pattern_semi_list opt_semi4 TCBracket
      { PatList (($1, $2 @ $3, $4)) }

 | TOBracketPipe pattern_semi_list opt_semi TPipeCBracket
      { PatTodo }
 | TOBracketPipe TPipeCBracket
      { PatTodo }

 /*(* note that let (x:...) a =  will trigger this rule *)*/
 | TOParen pattern TColon core_type TCParen
      { PatTyped ($1, $2, $3, $4, $5) }

 /*(* name tag extension *)*/
 | name_tag
      { PatTodo }
 /*(* range extension *)*/
 | TChar TDotDot TChar  
    { PatTodo }

 | TOParen pattern TCParen
    { ParenPat ($1, $2, $3) }


lbl_pattern_list:
 | label_longident TEq pattern               {[Left (PatField ($1, $2, $3))] }
 | label_longident                           {[Left (PatImplicitField ($1))]  }
 | lbl_pattern_list TSemiColon   label_longident TEq pattern 
     { $1 @ [Right $2; Left (PatField ($3, $4, $5))] }
 | lbl_pattern_list TSemiColon   label_longident       
     { $1 @ [Right $2; Left (PatImplicitField ($3))] }


record_pattern_end:
 | opt_semi                                    { }
 /*(* new 3.12 feature! *)*/
 | TSemiColon TUnderscore opt_semi              { }


pattern_semi_list:
 | pattern                                     { [Left $1] }
 | pattern_semi_list TSemiColon pattern        { $1 @[Right $2; Left $3] }

pattern_comma_list:
 | pattern_comma_list TComma pattern            { $1 @ [Right $2; Left $3] }
 | pattern TComma pattern                       { [Left $1; Right $2; Left $3] }


signed_constant:
 | constant       { C2 $1 }
 | TMinus TInt    { CMinus ($1, Int $2) }
 | TMinus TFloat  { CMinus ($1, Float $2) }
 | TPlus TInt     { CPlus ($1, Int $2) }
 | TPlus TFloat   { CPlus ($1, Float $2) }

/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

type_constraint:
 | TColon core_type           { }
 
 /*(* object cast extension *)*/
 | TColonGreater core_type    { }

/*(*----------------------------*)*/
/*(*2 Types definitions *)*/
/*(*----------------------------*)*/

type_declarations:
 | type_declaration                            { [Left $1] }
 | type_declarations Tand type_declaration     { $1 @ [Right $2; Left $3] }

type_declaration:
  type_parameters TLowerIdent type_kind /*(*TODO constraints*)*/
   { 
     match $3 with
     | None -> 
         TyAbstract ($1, Name $2)
     | Some (tok_eq, type_kind) ->
         TyDef ($1, Name $2, tok_eq, type_kind)
   }


type_kind:
 | /*(*empty*)*/
      { None }
 | TEq core_type
      { Some ($1, TyCore $2) }
 | TEq constructor_declarations
      { Some ($1, TyAlgebric $2) }
 | TEq /*(*TODO private_flag*)*/ TPipe constructor_declarations
      { Some ($1, TyAlgebric (Right $2::$3)) }
 | TEq /*(*TODO private_flag*)*/ TOBrace label_declarations opt_semi2 TCBrace
      { Some ($1, TyRecord ($2, ($3 @ $4), $5)) }



constructor_declarations:
 | constructor_declaration                     { [Left $1] }
 | constructor_declarations TPipe constructor_declaration 
     { $1 @ [Right $2; Left $3] }

constructor_declaration:
    constr_ident constructor_arguments          { Name $1, $2 }

constructor_arguments:
 | /*(*empty*)*/                            { NoConstrArg }
 | Tof core_type_list                       { Of ($1, $2) }


type_parameters:
 |  /*(*empty*)*/                              { TyNoParam  }
 | type_parameter                              { TyParam1 $1 }
 | TOParen type_parameter_list TCParen         { TyParamMulti (($1, $2, $3)) }

type_parameter_list:
 | type_parameter                               { [Left $1] }
 | type_parameter_list TComma type_parameter    { $1 @ [Right $2; Left $3] }

type_parameter:
  /*(*TODO type_variance*)*/ TQuote ident   { ($1, Name $2) }



label_declarations:
 | label_declaration                           { [Left $1] }
 | label_declarations TSemiColon label_declaration   { $1 @[Right $2; Left $3]}

label_declaration:
  mutable_flag label TColon poly_type          
   { 
     {
       fld_mutable = $1;
       fld_name = Name $2;
       fld_tok = $3;
       fld_type = $4;
     }
   }

mutable_flag:
 | /*(*empty*)*/       { None }
 | Tmutable            { Some $1 }


/*(*----------------------------*)*/
/*(*2 Types expressions *)*/
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
     { TyFunction ($3, $4, $5) (* TODO $1 $2 *)  }
 | TQuestion TLowerIdent TColon core_type2 TArrow core_type2
     { TyFunction ($4, $5, $6) (* TODO $1 $2 *)  }
 /*(* pad: only because of lexer hack around labels *)*/
 | TOptLabelDecl                core_type2 TArrow core_type2
     { TyFunction ($2, $3, $4) (* TODO $1 $2 *)  }


simple_core_type_or_tuple:
 | simple_core_type                          { $1 }
 | simple_core_type TStar core_type_list     { TyTuple (Left $1::Right $2::$3) }


simple_core_type:
 | simple_core_type2  %prec below_SHARP
      { $1 }
 /*(* weird diff between 'Foo of a * b' and 'Foo of (a * b)' *)*/
 | TOParen core_type_comma_list TCParen %prec below_SHARP
      { TyTuple2 (($1, $2, $3)) }

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
 | TOBracket TPipe row_field_list TCBracket
      { TyTodo }
 | TOBracket tag_field TCBracket
      { TyTodo }

 /*(* objects types *)*/
  | TLess meth_list TGreater
      { TyTodo }
  | TLess TGreater
      { TyTodo }


core_type_comma_list:
 | core_type                                  { [Left $1] }
 | core_type_comma_list TComma core_type      { $1 @ [Right $2; Left $3] }

core_type_list:
  | simple_core_type                         { [Left $1] }
  | core_type_list TStar simple_core_type    { $1 @ [Right $2; Left $3] }

meth_list:
  | field TSemiColon meth_list                      { }
  | field opt_semi                              {  }
  | TDotDot                                      {  }

field:
    label TColon poly_type             { }

/*(*----------------------------*)*/
/*(*2 Misc *)*/
/*(*----------------------------*)*/

poly_type:
 | core_type
     { $1 }


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
/*(*1 Let/Fun definitions *)*/
/*(*************************************************************************)*/

let_bindings:
 | let_binding                           { [Left $1] }
 | let_bindings Tand let_binding         { $1 @ [Right $2; Left $3] }


let_binding:
 | val_ident fun_binding
      { 
        let (params, (teq, body)) = $2 in
        LetClassic {
          l_name = Name $1;
          l_params = params;
          l_tok = teq;
          l_body = body;
        }
      }
 | pattern TEq seq_expr
      { LetPattern ($1, $2, $3) }


fun_binding:
 | strict_binding { $1 }
 /*(* let x arg1 arg2 : t = e *)*/
 | type_constraint TEq seq_expr { [], ($2, $3) (* TODO return triple with $1*)}

strict_binding:
 /*(* simple values, e.g. 'let x = 1' *)*/
 | TEq seq_expr  { [], ($1, $2) }
 /*(* function values, e.g. 'let x a b c = 1' *)*/
 | labeled_simple_pattern fun_binding { let (args, body) = $2 in $1::args, body }

fun_def:
 | match_action                    { [], $1 }
 | labeled_simple_pattern fun_def  { let (args, body) = $2 in $1::args, body }


labeled_simple_pattern:
  | simple_pattern { ParamPat $1 }
  | label_pattern  { $1 }

opt_default:
 | /*(*empty*)*/           { None  }
 | TEq seq_expr            { Some ($1, $2) }

rec_flag:
 | /*(*empty*)*/   { None }
 | Trec            { Some $1 }

/*(*----------------------------*)*/
/*(*2 Labels *)*/
/*(*----------------------------*)*/

label_pattern:
  | TTilde label_var
      { ParamTodo }
  /*(* ex: let x ~foo:a *)*/
  | TLabelDecl simple_pattern
      { ParamTodo }
  | TTilde TOParen label_let_pattern TCParen
      { ParamTodo }
  | TQuestion TOParen label_let_pattern opt_default TCParen
      { ParamTodo }
  | TQuestion label_var
      { ParamTodo }

label_let_pattern:
 | label_var                   { }
 | label_var TColon core_type  { }

/*(*************************************************************************)*/
/*(*1 Classes *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(*2 Class types *)*/
/*(*----------------------------*)*/
class_description:
 virtual_flag class_type_parameters TLowerIdent TColon class_type
  { }

class_type_declaration:
  virtual_flag class_type_parameters TLowerIdent TEq class_signature
  { }

class_type:
  | class_signature { }
  | simple_core_type_or_tuple TArrow class_type { }

class_signature:
  /*  LBRACKET core_type_comma_list RBRACKET clty_longident
      {  }
  */
  | clty_longident
      {  }
  | Tobject class_sig_body Tend
      {  }

class_sig_body:
    class_self_type class_sig_fields { }

class_self_type:
    TOParen core_type TCParen
      { }
  | /*(*empty*)*/ {  }

class_sig_fields:
  | class_sig_fields Tinherit class_signature    {  }
  | class_sig_fields virtual_method_type        {  }
  | class_sig_fields method_type                {  }

  | class_sig_fields Tval value_type            {  }
/*
  | class_sig_fields Tconstraint constrain       {  }
*/
  | /*(*empty*)*/                               { }

method_type:
  | Tmethod private_flag label TColon poly_type { }

virtual_method_type:
  | Tmethod Tprivate Tvirtual label TColon poly_type
      {  }
  | Tmethod Tvirtual private_flag label TColon poly_type
      {  }

value_type:
  | Tvirtual mutable_flag label TColon core_type
      { }
  | Tmutable virtual_flag label TColon core_type
      {  }
  | label TColon core_type
      {  }

/*(*----------------------------*)*/
/*(*2 Class expressions *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(*2 Class definitions *)*/
/*(*----------------------------*)*/

class_declaration:
    virtual_flag class_type_parameters TLowerIdent class_fun_binding
      { }

class_type_parameters:
  | /*(*empty*)*/                                   { }
  | TOBracket type_parameter_list TCBracket       { }

class_fun_binding:
  | TEq class_expr
      { }
  | labeled_simple_pattern class_fun_binding
      { }

class_expr:
  | class_simple_expr
      { }
  | Tfun class_fun_def
      { }
  | class_simple_expr simple_labeled_expr_list
      { }
  | Tlet rec_flag let_bindings Tin class_expr
      { }

class_simple_expr:
  | TOBracket core_type_comma_list TCBracket class_longident
      { }
  | class_longident
      { }
  | Tobject class_structure Tend
      { }
/* TODO
  | TOParen class_expr TColon class_type TCParen
      { }
*/
  | TOParen class_expr TCParen
      { }

class_fun_def:
  | labeled_simple_pattern TArrow class_expr
      { }
  | labeled_simple_pattern class_fun_def
      { }

class_structure:
    class_self_pattern class_fields
      { }

class_self_pattern:
  | TOParen pattern TCParen
      { }
  | TOParen pattern TColon core_type TCParen
      { }
  | /*(*empty*)*/
      { }



class_fields:
  | /*(*empty*)*/
      { }
  | class_fields Tinherit override_flag class_expr parent_binder
      { }
  | class_fields Tval virtual_value
      { }
  | class_fields Tval value
      { }
  | class_fields virtual_method
      { }
  | class_fields concrete_method
      { }
/* TODO
  | class_fields Tconstraint constrain
      { }
*/
  | class_fields Tinitializer seq_expr
      { }


parent_binder:
  | Tas TLowerIdent
          { }
  | /* empty */
          { }


virtual_value:
  | override_flag Tmutable Tvirtual label TColon core_type
      { }
  | Tvirtual mutable_flag label TColon core_type
      { }

value:
  | override_flag mutable_flag label TEq seq_expr
      { }
  | override_flag mutable_flag label type_constraint TEq seq_expr
      { }

virtual_method:
  | Tmethod override_flag Tprivate Tvirtual label TColon poly_type
      { }
  | Tmethod override_flag Tvirtual private_flag label TColon poly_type
      { }

concrete_method:
  | Tmethod override_flag private_flag label strict_binding
      { }
  | Tmethod override_flag private_flag label TColon poly_type TEq seq_expr
      { }



virtual_flag:
 | /*(* empty*)*/                               { }
 | Tvirtual                                     { }

/*(* 3.12? *)*/
override_flag:
 | /*(*empty*)*/                                 { }
 | TBang                                        { }

private_flag:
    /* empty */                                 { }
  | Tprivate                                     { }

/*(*************************************************************************)*/
/*(*1 Modules *)*/
/*(*************************************************************************)*/

module_binding:
 | TEq module_expr
     { Some ($1, $2) }
 | TOParen TUpperIdent TColon module_type TCParen module_binding
     { None }
 | TColon module_type TEq module_expr
     { (* TODO $1 *) Some ($3, $4) }

module_declaration:
 | TColon module_type
      { }
 | TOParen TUpperIdent TColon module_type TCParen module_declaration
     { }

/*(*----------------------------*)*/
/*(*2 Module types *)*/
/*(*----------------------------*)*/

module_type:
 | mty_longident
      { }
 | Tsig signature Tend
      { }
 | Tfunctor TOParen TUpperIdent TColon module_type TCParen TArrow module_type
      %prec below_WITH
      { }
 | module_type Twith with_constraints
     { }
 | TOParen module_type TCParen
      { }


with_constraint:
 | Ttype type_parameters label_longident with_type_binder core_type 
    /*constraints*/
   { }

with_type_binder:
 | TEq          {  }
 | TEq Tprivate  {  }

/*(*----------------------------*)*/
/*(*2 Module expressions *)*/
/*(*----------------------------*)*/

module_expr:
  /*(* when just do a module aliasing *)*/
  | mod_longident
      { ModuleName $1 }
  /*(* nested modules *)*/
  | Tstruct structure Tend
      { ModuleStruct ($1, to_item $2, $3) }
  /*(* functor definition *)*/
  | Tfunctor TOParen TUpperIdent TColon module_type TCParen TArrow module_expr
      { ModuleTodo }
  /*(* module/functor application *)*/
  | module_expr TOParen module_expr TCParen
      { ModuleTodo }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

opt_semi:
 | /*(*empty*)*/    { [] }
 | TSemiColon       { [Right $1] }

opt_semi2:
 | /*(*empty*)*/    { [] }
 | TSemiColon       { [Right $1] }

opt_semi3:
 | /*(*empty*)*/    { [] }
 | TSemiColon       { [Right $1] }

opt_semi4:
 | /*(*empty*)*/    { [] }
 | TSemiColon       { [Right $1] }

opt_bar:
 | /*(*empty*)*/    { [] }
 | TPipe            { [Right $1] }

with_constraints:
 | with_constraint                             { [Left $1] }
 | with_constraints Tand with_constraint        { $1 @ [Right $2; Left $3] }

class_declarations:
  | class_declarations TAnd class_declaration   { }
  | class_declaration                           { }

class_descriptions:
  | class_descriptions TAnd class_description   { }
  | class_description                           { }

class_type_declarations:
  | class_type_declarations TAnd class_type_declaration  {  }
  | class_type_declaration                               { }
