%{
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2014 Facebook
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2002 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Ast_cpp
open Parser_cpp_mly_helper

(* see todo_mly for stuff temporarily commented out *)

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
/*
(* Some tokens below are not even used in this file because they are filtered
 * in some intermediate phases (e.g. the comment tokens). Some tokens
 * also appear only here and are not in the lexer because they are
 * created in some intermediate phases. They are called "fresh" tokens
 * and always contain a '_' in their name.
 *)*/

/*(* unrecognized token, will generate parse error *)*/
%token <Parse_info.info> TUnknown

%token <Parse_info.info> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/
/*
(* coupling: Token_helpers.is_real_comment and other related functions.
 * disappear in parse_cpp.ml via TH.is_comment in lexer_function
 *)*/
%token <Parse_info.info> TCommentSpace TCommentNewline TComment

/*(* fresh_token: cppext: appears after parsing_hack_pp and disappear *)*/
%token <(Token_cpp.cppcommentkind * Parse_info.info)> TComment_Pp
/*(* fresh_token: c++ext: appears after parsing_hack_pp and disappear *)*/
%token <(Token_cpp.cpluspluscommentkind * Parse_info.info)> TComment_Cpp

/*(*-----------------------------------------*)*/
/*(*2 The C tokens *)*/
/*(*-----------------------------------------*)*/

%token <string * Parse_info.info>                       TInt
%token <(string * Ast_cpp.floatType) * Parse_info.info> TFloat
%token <(string * Ast_cpp.isWchar) * Parse_info.info>   TChar TString

%token <string * Parse_info.info> TIdent 
/*(* fresh_token: appear after some fix_tokens in parsing_hack.ml *)*/
%token <string * Parse_info.info> TIdent_Typedef

/*
(* coupling: some tokens like TOPar and TCPar are used as synchronisation point 
 * in parsing_hack.ml. So if you define a special token like TOParDefine and
 * TCParEOL, then you must take care to also modify token_helpers.ml
 *)*/
%token <Parse_info.info> TOPar TCPar TOBrace TCBrace TOCro TCCro 

%token <Parse_info.info> TDot TComma TPtrOp     TInc TDec
%token <Ast_cpp.assignOp * Parse_info.info> TAssign 
%token <Parse_info.info> TEq  TWhy  TTilde TBang  TEllipsis  TCol  TPtVirg
%token <Parse_info.info> 
  TOrLog TAndLog TOr TXor TAnd  TEqEq TNotEq TInfEq TSupEq
  TShl TShr 
  TPlus TMinus TMul TDiv TMod 

/*(*c++ext: see also TInf2 and TSup2 *)*/
%token <Parse_info.info> TInf TSup 

%token <Parse_info.info>
  Tchar Tshort Tint Tdouble Tfloat Tlong Tunsigned Tsigned Tvoid
  Tauto Tregister Textern Tstatic 
  Ttypedef 
  Tconst Tvolatile
  Tstruct Tunion Tenum 
  Tbreak Telse Tswitch Tcase Tcontinue Tfor Tdo Tif  Twhile Treturn
  Tgoto Tdefault
  Tsizeof  

/*(* C99 *)*/
%token <Parse_info.info> Trestrict

/*(*-----------------------------------------*)*/
/*(*2 gccext: extra tokens *)*/
/*(*-----------------------------------------*)*/
%token <Parse_info.info> Tasm Ttypeof
/*(* less: disappear in parsing_hacks_pp, not present in AST for now *)*/
%token <Parse_info.info> Tattribute
/*(* also c++ext: *)*/
%token <Parse_info.info> Tinline 

/*(*-----------------------------------------*)*/
/*(*2 cppext: extra tokens *)*/
/*(*-----------------------------------------*)*/

/*(* cppext: #define  *)*/
%token <Parse_info.info> TDefine
%token <(string * Parse_info.info)> TDefParamVariadic
/*(* transformed in TCommentSpace and disappear in parsing_hack.ml *)*/
%token <Parse_info.info> TCppEscapedNewline 
/*(* fresh_token: appear after fix_tokens_define in parsing_hack_define.ml *)*/
%token <(string * Parse_info.info)> TIdent_Define
%token <Parse_info.info> TOPar_Define
%token <Parse_info.info> TCommentNewline_DefineEndOfMacro
%token <Parse_info.info> TOBrace_DefineInit

/*(* cppext: #include  *)*/
%token <(string * string * Parse_info.info)> TInclude

/*(* cppext: #ifdef *)*/
/*(* coupling: Token_helpers.is_cpp_instruction *)*/
%token <Parse_info.info>          TIfdef TIfdefelse TIfdefelif TEndif
%token <(bool * Parse_info.info)> TIfdefBool TIfdefMisc TIfdefVersion

/*(* cppext: other *)*/
%token <string * Parse_info.info> TUndef
%token <Parse_info.info> TCppDirectiveOther

/*(* cppext: special macros *)*/
/*(* fresh_token: appear after fix_tokens in parsing_hacks_pp.ml *)*/
%token <Parse_info.info>            TIdent_MacroStmt
%token <Parse_info.info>            TIdent_MacroString 
%token <(string * Parse_info.info)> TIdent_MacroIterator
%token <(string * Parse_info.info)> TIdent_MacroDecl
%token <Parse_info.info>            Tconst_MacroDeclConst 

/*(* fresh_token: appear after parsing_hack_pp.ml, alt to TIdent_MacroTop *)*/
%token <Parse_info.info> TCPar_EOL
/*(* fresh_token: appear after parsing_hack_pp.ml *)*/
%token <Parse_info.info> TAny_Action

/*(*-----------------------------------------*)*/
/*(*2 c++ext: extra tokens *)*/
/*(*-----------------------------------------*)*/
%token <Parse_info.info>
   Tclass Tthis 
   Tnew Tdelete 
   Ttemplate Ttypeid Ttypename 
   Tcatch Ttry Tthrow 
   Toperator 
   Tpublic Tprivate Tprotected    Tfriend 
   Tvirtual 
   Tnamespace Tusing 
   Tbool    Tfalse Ttrue 
   Twchar_t 
   Tconst_cast Tdynamic_cast Tstatic_cast Treinterpret_cast 
   Texplicit Tmutable 
   Texport
%token <Parse_info.info> TPtrOpStar TDotStar

%token <Parse_info.info> TColCol 

/*(* fresh_token: for constructed object, in parsing_hacks_cpp.ml *)*/
%token <Parse_info.info> TOPar_CplusplusInit
/*(* fresh_token: for template *)*/
%token <Parse_info.info> TInf_Template TSup_Template
/*(* fresh_token: for new[] delete[] *)*/
%token <Parse_info.info> TOCro_new TCCro_new
/*(* fresh_token: for pure virtual method. TODO add stuff in parsing_hack *)*/
%token <Parse_info.info> TInt_ZeroVirtual
/*(* fresh_token: why can't use TypedefIdent? conflict? *)*/
%token <string * Parse_info.info> TIdent_ClassnameInQualifier
/*(* fresh_token: appears after solved if next token is a typedef *)*/
%token <string * Parse_info.info> TIdent_ClassnameInQualifier_BeforeTypedef
/*(* fresh_token: just before <> *)*/
%token <string * Parse_info.info> TIdent_Templatename
/*(* for templatename as qualifier, before a '::' TODO write heuristic! *)*/
%token <string * Parse_info.info> TIdent_TemplatenameInQualifier
/*(* fresh_token: appears after solved if next token is a typedef *)*/
%token <string * Parse_info.info> TIdent_TemplatenameInQualifier_BeforeTypedef
/*(* fresh_token: for methods with same name as classname *)*/
%token <string * Parse_info.info> TIdent_Constructor
/*(* for cast_constructor, before a '(', unused for now *)*/
%token <string * Parse_info.info> TIdent_TypedefConstr
/*(* fresh_token: for constructed (basic) objects *)*/
%token <Parse_info.info> 
  Tchar_Constr Tint_Constr Tfloat_Constr Tdouble_Constr Twchar_t_Constr
  Tshort_Constr Tlong_Constr Tbool_Constr
  Tsigned_Constr Tunsigned_Constr
/*(* fresh_token: appears after solved if next token is a typedef *)*/
%token <Parse_info.info> TColCol_BeforeTypedef

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc LOW_PRIORITY_RULE
/*(* see conflicts.txt *)*/
%nonassoc Telse


%left TOrLog
%left TAndLog
%left TOr
%left TXor
%left TAnd 
%left TEqEq TNotEq
%left TInf TSup TInfEq TSupEq 
%left TShl TShr
%left TPlus TMinus
%left TMul TDiv TMod 

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/
%start main toplevel statement expr type_id

%type <Ast_cpp.program> main
%type <Ast_cpp.toplevel option> toplevel
%type <Ast_cpp.statement> statement
%type <Ast_cpp.expression> expr
%type <Ast_cpp.fullType> type_id
%type <(Ast_cpp.name) * (Ast_cpp.fullType -> Ast_cpp.fullType)> declarator
%type <(Ast_cpp.name)> type_cplusplus_id
%%

/*(*************************************************************************)*/
/*(*1 TOC *)*/
/*(*************************************************************************)*/
/*
(* translation_unit (obsolete)
 * 
 * ident
 * expression
 * statement
 * types with 
 *   - left part (type_spec, qualif, template and its arguments), 
 *   - right part (declarator, abstract declarator)
 *   - aux part (parameters)
 * class/struct
 * enum
 * declaration, storage, initializers
 * block_declaration
 * cpp directives
 * toplevel (= start grammar rule)
 * 
 * generic workarounds (obrace, cbrace for context setting)
 * xxx_list, xxx_opt
 *)*/
/*(*************************************************************************)*/
/*(*1 translation_unit (unused) *)*/
/*(*************************************************************************)*/

/*(* no more used now that use error recovery, but good to keep *)*/
main:  
 | translation_unit EOF     { $1 }

translation_unit: 
 | external_declaration                      { [DeclElem $1] }
 | translation_unit external_declaration     { $1 @ [DeclElem $2] }

external_declaration: 
 | function_definition            { Func (FunctionOrMethod $1) }
 | block_declaration              { BlockDecl $1 }

/*(*************************************************************************)*/
/*(*1 Ident, scope *)*/
/*(*************************************************************************)*/

id_expression:
 | unqualified_id { noQscope, $1 }
 | qualified_id { $1 }

/*
(* todo:
 * ~id class_name,  conflict  IdDestructor TODO
 * template-id,   conflict
 *)*/
unqualified_id:
 | TIdent                 { IdIdent $1 }
 | operator_function_id   { $1 }
 | conversion_function_id { $1 }

operator_function_id: 
 | Toperator operator_kind
    { IdOperator ($1, $2) }

conversion_function_id:
 | Toperator conversion_type_id
    { IdConverter ($1, $2) }
/*
(* no deref getref operator (cos ambiguity with Mul and And), 
 * no unaryplus/minus op either 
 *)*/
operator_kind:
 /*(* != == *)*/
 | TEqEq  { BinaryOp (Logical Eq),    [$1] }
 | TNotEq { BinaryOp (Logical NotEq), [$1] }
 /*(* =    +=   -=   *=   /=   %=       ^=   &=   |=   >>=  <<=   *)*/
 | TEq     { AssignOp SimpleAssign, [$1] }     
 | TAssign { AssignOp (fst $1), [snd $1] } 
 /*(* ! ~ *)*/
 | TTilde { UnaryTildeOp, [$1] }
 | TBang  { UnaryNotOp,   [$1] }
 /*(* , *)*/
 | TComma { CommaOp,  [$1] }
 /*(* +    -    *    /    %  *)*/
 | TPlus  { BinaryOp (Arith Plus),  [$1] }  
 | TMinus { BinaryOp (Arith Minus), [$1] }
 | TMul   { BinaryOp (Arith Mul),   [$1] }  
 | TDiv   { BinaryOp (Arith Div),   [$1] }
 | TMod   { BinaryOp (Arith Mod),   [$1] }
 /*(* ^ & |     <<   >>  *)*/
 | TOr   { BinaryOp (Arith Or),  [$1] }  
 | TXor  { BinaryOp (Arith Xor), [$1] } 
 | TAnd  { BinaryOp (Arith And), [$1]  } 
 | TShl   { BinaryOp (Arith DecLeft), [$1] }  
 | TShr   { BinaryOp (Arith DecRight), [$1] }
 /*(* &&   || *)*/
 | TOrLog  { BinaryOp (Logical OrLog), [$1] } 
 | TAndLog { BinaryOp (Logical AndLog), [$1] }
 /*(* < >  <=   >=  *)*/
 | TInf   { BinaryOp (Logical Inf), [$1] }  
 | TSup   { BinaryOp (Logical Sup), [$1] }
 | TInfEq { BinaryOp (Logical InfEq), [$1] }   
 | TSupEq { BinaryOp (Logical SupEq), [$1] }
 /*(* ++   -- *)*/
 | TInc   { FixOp Inc, [$1] }  
 | TDec   { FixOp Dec, [$1] }
 /*(* ->*  -> *) */
 | TPtrOpStar { PtrOpOp PtrStarOp, [$1] }  
 | TPtrOp     { PtrOpOp PtrOp,     [$1] }
 /*(* () [] (double tokens) *)*/
 | TOPar TCPar { AccessOp ParenOp, [$1;$2] }
 | TOCro TCCro { AccessOp ArrayOp, [$1;$2] }
 /*(* new delete *)*/
 | Tnew    { AllocOp NewOp,    [$1] } 
 | Tdelete { AllocOp DeleteOp, [$1] }
 /*(*new[] delete[] (tripple tokens) *)*/
 | Tnew    TOCro_new TCCro_new { AllocOp NewArrayOp,    [$1;$2;$3] }
 | Tdelete TOCro_new TCCro_new { AllocOp DeleteArrayOp, [$1;$2;$3] }



qualified_id: 
 | nested_name_specifier /*(*templateopt*)*/ unqualified_id 
   { $1, $2 }

nested_name_specifier: 
 | class_or_namespace_name_for_qualifier TColCol nested_name_specifier_opt 
   { ($1, $2)::$3 }

/*(* context dependent *)*/
class_or_namespace_name_for_qualifier:
 | TIdent_ClassnameInQualifier 
     { QClassname $1 }
 | TIdent_TemplatenameInQualifier 
    TInf_Template template_argument_list TSup_Template
     { QTemplateId ($1, ($2, $3, $4)) }


/*
(* context dependent: in the original grammar there was one rule
 * for each names (e.g. typedef_name:, enum_name:, class_name:) but 
 * we don't have such contextual information and we can merge 
 * those rules anyway without introducing conflicts.
 *)*/
enum_name_or_typedef_name_or_simple_class_name:
 | TIdent_Typedef { $1 }
/*(* used only with namespace/using rules. We use Tclassname for stuff
   * like std::... todo? or just TIdent_Typedef? *)*/
namespace_name:
 | TIdent { $1 }

/*(*----------------------------*)*/
/*(*2 workarounds *)*/
/*(*----------------------------*)*/
nested_name_specifier2: 
 | class_or_namespace_name_for_qualifier2 
    TColCol_BeforeTypedef nested_name_specifier_opt2 
     { ($1, $2)::$3 }

class_or_namespace_name_for_qualifier2:
 | TIdent_ClassnameInQualifier_BeforeTypedef 
     { QClassname $1  }
 | TIdent_TemplatenameInQualifier_BeforeTypedef 
    TInf_Template template_argument_list TSup_Template
     { QTemplateId ($1, ($2, $3, $4)) }

/*
(* Why this ? Why not s/ident/TIdent ? cos there is multiple namespaces in C, 
 * so a label can have the same name that a typedef, same for field and tags
 * hence sometimes the use of ident instead of TIdent.
 *)*/
ident: 
 | TIdent         { $1 }
 | TIdent_Typedef { $1 }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr: 
 | assign_expr             { $1 }
 | expr TComma assign_expr { mk_e (Sequence ($1,$3)) [$2] }

/*(* bugfix: in C grammar they put 'unary_expr', but in fact it must be 
   * 'cast_expr', otherwise (int * ) xxx = &yy; is not allowed
   *)*/
assign_expr: 
 | cond_expr                     { $1 }
 | cast_expr TAssign assign_expr { mk_e(Assignment ($1,fst $2,$3)) [snd $2]}
 | cast_expr TEq     assign_expr { mk_e(Assignment ($1,SimpleAssign,$3)) [$2]}
 /*(*c++ext: *)*/
 | Tthrow assign_expr_opt        { mk_e (Throw $2) [$1] }

/*(* gccext: allow optional then part hence opt_expr 
   * bugfix: in C grammar they put 'TCol cond_expr', but in fact it must be
   * 'assign_expr', otherwise   pnp ? x : x = 0x388  is not allowed
   *)*/
cond_expr: 
 | arith_expr   { $1 }
 | arith_expr TWhy expr_opt TCol assign_expr 
     { mk_e (CondExpr ($1,$3,$5)) [$2;$4] } 


arith_expr: 
 | pm_expr                     { $1 }
 | arith_expr TMul    arith_expr { mk_e(Binary ($1, Arith Mul,      $3)) [$2] }
 | arith_expr TDiv    arith_expr { mk_e(Binary ($1, Arith Div,      $3)) [$2] }
 | arith_expr TMod    arith_expr { mk_e(Binary ($1, Arith Mod,      $3)) [$2] }

 | arith_expr TPlus   arith_expr { mk_e(Binary ($1, Arith Plus,     $3)) [$2] }
 | arith_expr TMinus  arith_expr { mk_e(Binary ($1, Arith Minus,    $3)) [$2] }
 | arith_expr TShl    arith_expr { mk_e(Binary ($1, Arith DecLeft,  $3)) [$2] }
 | arith_expr TShr    arith_expr { mk_e(Binary ($1, Arith DecRight, $3)) [$2] }
 | arith_expr TInf    arith_expr { mk_e(Binary ($1, Logical Inf,    $3)) [$2] }
 | arith_expr TSup    arith_expr { mk_e(Binary ($1, Logical Sup,    $3)) [$2] }
 | arith_expr TInfEq  arith_expr { mk_e(Binary ($1, Logical InfEq,  $3)) [$2] }
 | arith_expr TSupEq  arith_expr { mk_e(Binary ($1, Logical SupEq,  $3)) [$2] }
 | arith_expr TEqEq   arith_expr { mk_e(Binary ($1, Logical Eq,     $3)) [$2] }
 | arith_expr TNotEq  arith_expr { mk_e(Binary ($1, Logical NotEq,  $3)) [$2] }
 | arith_expr TAnd    arith_expr { mk_e(Binary ($1, Arith And,      $3)) [$2] }
 | arith_expr TOr     arith_expr { mk_e(Binary ($1, Arith Or,       $3)) [$2] }
 | arith_expr TXor    arith_expr { mk_e(Binary ($1, Arith Xor,      $3)) [$2] }
 | arith_expr TAndLog arith_expr { mk_e(Binary ($1, Logical AndLog, $3)) [$2] }
 | arith_expr TOrLog  arith_expr { mk_e(Binary ($1, Logical OrLog,  $3)) [$2] }

pm_expr: 
 | cast_expr { $1 }
 /*(*c++ext: .* and ->*, note that not next to . and -> and take expr *)*/
 | pm_expr TDotStar   cast_expr
     { mk_e(RecordStarAccess   ($1,$3)) [$2]}
 | pm_expr TPtrOpStar cast_expr
     { mk_e(RecordPtStarAccess ($1,$3)) [$2]}

cast_expr: 
 | unary_expr                        { $1 }
 | TOPar type_id TCPar cast_expr { mk_e(Cast (($1, $2, $3), $4)) noii }

unary_expr: 
 | postfix_expr                    { $1 }
 | TInc unary_expr                 { mk_e(Infix ($2, Inc))    [$1] }
 | TDec unary_expr                 { mk_e(Infix ($2, Dec))    [$1] }
 | unary_op cast_expr              { mk_e(Unary ($2, fst $1)) [snd $1] }
 | Tsizeof unary_expr              { mk_e(SizeOfExpr ($1, $2)) noii }
 | Tsizeof TOPar type_id TCPar { mk_e(SizeOfType ($1, ($2, $3, $4))) noii }
 /*(*c++ext: *)*/
 | new_expr      { $1 }
 | delete_expr   { $1 }

unary_op: 
 | TAnd   { GetRef,     $1 }
 | TMul   { DeRef,      $1 }
 | TPlus  { UnPlus,     $1 }
 | TMinus { UnMinus,    $1 }
 | TTilde { Tilde,      $1 }
 | TBang  { Not,        $1 }
 /*(* gccext: have that a lot in old kernel to get address of local label.
    * cf gcc manual "local labels as values".
    *)*/
 | TAndLog { GetRefLabel, $1 }


postfix_expr: 
 | primary_expr               { $1 }
 | postfix_expr TOCro expr TCCro                
     { mk_e(ArrayAccess ($1, ($2, $3, $4))) noii }
 | postfix_expr TOPar argument_list_opt TCPar  
     { mk_e(mk_funcall $1 ($2, $3, $4)) noii }

 /*(*c++ext: ident is now a id_expression *)*/
 | postfix_expr TDot   template_opt tcolcol_opt  id_expression
     { let name = ($4, fst $5, snd $5) in mk_e(RecordAccess ($1,name)) [$2] }
 | postfix_expr TPtrOp template_opt tcolcol_opt id_expression  
     { let name = ($4, fst $5, snd $5) in mk_e(RecordPtAccess($1,name)) [$2] }

 | postfix_expr TInc          { mk_e(Postfix ($1, Inc)) [$2] }
 | postfix_expr TDec          { mk_e(Postfix ($1, Dec)) [$2] }

 /*(* gccext: also called compound literals *)*/
 | compound_literal_expr { $1 }

 /*(* c++ext: *)*/
 | cast_operator_expr { $1 }
 | Ttypeid TOPar unary_expr TCPar { mk_e(TypeId ($1, ($2, Right $3, $4))) noii }
 | Ttypeid TOPar type_id    TCPar { mk_e(TypeId ($1, ($2, Left $3, $4))) noii }
 | cast_constructor_expr { $1 }


primary_expr: 
 /*(*c++ext: cf below now. old: TIdent { mk_e(Ident  (fst $1)) [snd $1] }  *)*/

 /*(* constants a.k.a literal *)*/
 | TInt    { mk_e(C (Int    (fst $1))) [snd $1] }
 | TFloat  { mk_e(C (Float  (fst $1))) [snd $1] }
 | TString { mk_e(C (String (fst $1))) [snd $1] }
 | TChar   { mk_e(C (Char   (fst $1))) [snd $1] }
 /*(*c++ext: *)*/
 | Ttrue   { mk_e(C (Bool false)) [$1] }
 | Tfalse  { mk_e(C (Bool false)) [$1] }

  /*(* forunparser: *)*/
 | TOPar expr TCPar { mk_e(ParenExpr ($1, $2, $3)) noii }  

 /*(* gccext: cppext: *)*/
 | string_elem string_list { mk_e(C (MultiString)) ($1 @ $2) }
 /*(* gccext: allow statement as expressions via ({ statement }) *)*/
 | TOPar compound TCPar    { mk_e(StatementExpr ($1, $2, $3)) noii }

 /*(* c++ext: *)*/
 | Tthis { mk_e(This $1) [] }
 /*(* contains identifier rule *)*/
 | primary_cplusplus_id { $1 }

/*(*----------------------------*)*/
/*(*2 c++ext: *)*/
/*(*----------------------------*)*/

/*(* can't factorize with following rule :(
   * | tcolcol_opt nested_name_specifier_opt TIdent
   *)*/
primary_cplusplus_id:
 | id_expression 
     { let name = (None, fst $1, snd $1) in 
       mk_e (Id (name, noIdInfo())) []  }
 | TColCol TIdent  
     { let name = Some $1, noQscope, IdIdent $2 in 
       mk_e (Id (name, noIdInfo())) [] }
 | TColCol operator_function_id 
     { let qop = $2 in
       let name = (Some $1, noQscope, qop) in 
       mk_e (Id (name, noIdInfo())) [] }
 | TColCol qualified_id 
     { let name = (Some $1, fst $2, snd $2) in 
       mk_e (Id (name, noIdInfo())) [] }

/*(*could use TInf here *)*/
cast_operator_expr: 
 | cpp_cast_operator TInf_Template type_id  TSup_Template TOPar expr TCPar 
     { mk_e (CplusplusCast ($1, ($2, $3, $4), ($5, $6, $7))) noii }
/*(* TODO: remove once we don't skip template arguments *)*/
 | cpp_cast_operator TOPar expr TCPar
     { mk_e ExprTodo noii }

/*(*c++ext:*)*/
cpp_cast_operator:
 | Tstatic_cast      { Static_cast, $1 }
 | Tdynamic_cast     { Dynamic_cast, $1 }
 | Tconst_cast       { Const_cast, $1 }
 | Treinterpret_cast { Reinterpret_cast, $1 }

/*
(* c++ext: cast with function syntax, and also constructor, but conflict
 * hence the TIdent_TypedefConstr. But it's simpler to just consider
 * this as a function call. A semantic analysis could infer it was
 * actually a ConstructedObject.
 * 
 * TODO: can have nested specifier before the typedefident ... so 
 * need a classname3?
*)*/
cast_constructor_expr:
 | TIdent_TypedefConstr TOPar argument_list_opt TCPar 
     { let name = None, noQscope, IdIdent $1 in
       let ft = nQ, (TypeName name, noii) in
       mk_e(ConstructedObject (ft, ($2, $3, $4))) noii  
     }
 | basic_type_2 TOPar argument_list_opt TCPar 
     { let ft = nQ, $1 in
       mk_e(ConstructedObject (ft, ($2, $3, $4))) noii
     }

/*(* c++ext: * simple case: new A(x1, x2); *)*/
new_expr:
 | tcolcol_opt Tnew new_placement_opt   new_type_id  new_initializer_opt
     { mk_e (New ($1, $2, $3, $4, $5)) noii  }
/*(* ambiguity then on the TOPar
 tcolcol_opt Tnew new_placement_opt TOPar type_id TCPar new_initializer_opt
  *)*/

delete_expr:
 | tcolcol_opt Tdelete cast_expr 
     { mk_e (Delete ($1, $3)) [$2] }
 | tcolcol_opt Tdelete TOCro_new TCCro_new cast_expr 
     { mk_e (DeleteArray ($1, $5)) [$2;$3;$4] }

new_placement: 
 | TOPar argument_list TCPar { ($1, $2, $3) }

new_initializer: 
 | TOPar argument_list_opt TCPar { ($1, $2, $3) }

/*(*----------------------------*)*/
/*(*2 gccext: *)*/
/*(*----------------------------*)*/

compound_literal_expr:
 | TOPar type_id TCPar TOBrace TCBrace 
     { mk_e(GccConstructor (($1, $2, $3), ($4, [], $5))) noii }
 | TOPar type_id TCPar TOBrace initialize_list gcc_comma_opt TCBrace
     { mk_e(GccConstructor (($1, $2, $3), ($4, List.rev $5, $7))) noii }

string_elem:
 | TString { [snd $1] }
 /*(* cppext:  ex= printk (KERN_INFO "xxx" UTS_RELEASE)  *)*/
 | TIdent_MacroString { [$1] }

/*(*----------------------------*)*/
/*(*2 cppext: *)*/
/*(*----------------------------*)*/

argument:
 | assign_expr { Left $1 }
/*(* cppext: *)*/
/*(* actually this can happen also when have a wrong typedef inference ...*)*/
 | type_id { Right (ArgType $1)  }
/* see todo_mly */

/*(*----------------------------*)*/
/*(*2 workarounds *)*/
/*(*----------------------------*)*/

/*(* would like evalInt $1 but require too much info *)*/
const_expr: cond_expr { $1  }

basic_type_2: 
 | Tchar_Constr    { (BaseType (IntType CChar)), [$1]}
 | Tint_Constr     { (BaseType (IntType (Si (Signed,CInt)))), [$1]}
 | Tfloat_Constr   { (BaseType (FloatType CFloat)),  [$1]}
 | Tdouble_Constr  { (BaseType (FloatType CDouble)), [$1] }

 | Twchar_t_Constr { (BaseType (IntType WChar_t)),         [$1] }

 | Tshort_Constr   { (BaseType (IntType (Si (Signed, CShort)))),  [$1] }
 | Tlong_Constr    { (BaseType (IntType (Si (Signed, CLong)))),   [$1] }
 | Tbool_Constr    { (BaseType (IntType CBool)),         [$1] }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

statement: 
 | compound        { Compound $1, noii }
 | expr_statement  { ExprStatement(fst $1), snd $1 }
 | labeled         { Labeled      (fst $1), snd $1 }
 | selection       { Selection    (fst $1), snd $1 }
 | iteration       { Iteration    (fst $1), snd $1 }
 | jump TPtVirg    { Jump         (fst $1), snd $1 @ [$2] }

 /*(* cppext: *)*/
 | TIdent_MacroStmt { MacroStmt, [$1] }

 /*
 (* cppext: c++ext: because of cpp, some stuff looks like declaration but are in
  * fact statement but too hard to figure out, and if parse them as
  * expression, then we force to have first decls and then exprs, then
  * will have a parse error. So easier to let mix decl/statement.
  * Moreover it helps to not make such a difference between decl and
  * statement for further coccinelle phases to factorize code.
  * 
  * update: now a c++ext and handle slightly differently. It's inlined
  * in statement instead of going through a stat_or_decl.
  *)*/
 | declaration_statement { $1 }

 /*(* gccext: if move in statement then can have r/r conflict with define *)*/
 | function_definition { NestedFunc $1, noii }

 /*(* c++ext: *)*/
 | try_block { $1 }


compound: 
 | TOBrace statement_list_opt TCBrace { ($1, $2, $3) }


expr_statement: 
 | expr_opt TPtVirg { $1, [$2] }

/*(* note that case 1: case 2: i++;    would be correctly parsed, but with 
   * a Case  (1, (Case (2, i++)))  :(  
   *)*/
labeled: 
 | ident            TCol statement   { Label (fst $1, $3),  [snd $1; $2] }
 | Tcase const_expr TCol statement   { Case ($2, $4),       [$1; $3] }
 | Tcase const_expr TEllipsis const_expr TCol statement 
     { CaseRange ($2, $4, $6), [$1;$3;$5] } /*(* gccext: allow range *)*/
 | Tdefault         TCol statement   { Default $3,             [$1; $2] } 

/*(* classic else ambiguity resolved by a %prec, see conflicts.txt *)*/
selection: 
 | Tif TOPar expr TCPar statement              %prec LOW_PRIORITY_RULE
     { If ($1, ($2, $3, $4), $5, None, (ExprStatement None, [])), noii }
 | Tif TOPar expr TCPar statement Telse statement 
     { If ($1, ($2, $3, $4), $5, Some $6, $7), noii }
 | Tswitch TOPar expr TCPar statement             
     { Switch ($1, ($2, $3, $4), $5), noii }

iteration: 
 | Twhile TOPar expr TCPar statement                             
     { While ($1, ($2, $3, $4), $5), noii }
 | Tdo statement Twhile TOPar expr TCPar TPtVirg                 
     { DoWhile ($1, $2, $3, ($4, $5, $6), $7), noii }
 | Tfor TOPar expr_statement expr_statement expr_opt TCPar statement
     { For ($1, ($2, ($3,$4, ($5, [])), $6), $7), noii }
 /*(* cppext: *)*/
 | TIdent_MacroIterator TOPar argument_list_opt TCPar statement
     { MacroIteration ($1, ($2, $3, $4), $5), noii }

/*(* the ';' in the caller grammar rule will be appended to the infos *)*/
jump: 
 | Tgoto ident  { Goto (fst $2),  [$1;snd $2] } 
 | Tcontinue    { Continue,       [$1] }
 | Tbreak       { Break,          [$1] }
 | Treturn      { Return,         [$1] } 
 | Treturn expr { ReturnExpr $2,  [$1] }
 | Tgoto TMul expr { GotoComputed $3, [$1;$2] }


/*(*----------------------------*)*/
/*(*2 cppext: *)*/
/*(*----------------------------*)*/

statement_list_opt:
 | /*(*empty*)*/ { [] }
 | statement_list { $1 }

statement_list:
 | statement_seq { [$1] }
 | statement_list statement_seq { $1 @ [$2] }

statement_seq:
 | statement { StmtElem $1 }
 /*(* cppext: *)*/
 | cpp_directive 
     { CppDirectiveStmt $1 }
 | cpp_ifdef_directive/*(* stat_or_decl_list ...*)*/  
     { IfdefStmt $1 }

/*(*----------------------------*)*/
/*(*2 c++ext: *)*/
/*(*----------------------------*)*/

declaration_statement:
 | block_declaration { DeclStmt $1, noii }

try_block: 
 | Ttry compound handler_list { Try ($1, $2, $3), noii }

handler: 
 | Tcatch TOPar exception_decl TCPar compound { ($1, ($2, $3, $4), $5) }

exception_decl:
 | parameter_decl { ExnDecl $1 }
 | TEllipsis      { ExnDeclEllipsis $1 }

/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------------------------------------*)*/
/*(*2 Type spec, left part of a type *)*/
/*(*-----------------------------------------------------------------------*)*/

/*(* in c++ grammar they put 'cv_qualifier' here but I prefer keep as before *)*/
type_spec:
 | simple_type_specifier { $1 }
 | elaborated_type_specifier { $1 }
 | enum_specifier  { Right3 $1, noii }
 | class_specifier { Right3 (StructDef $1), noii }


simple_type_specifier:
 | Tvoid                { Right3 (BaseType Void),            [$1] }
 | Tchar                { Right3 (BaseType (IntType CChar)), [$1]}
 | Tint                 { Right3 (BaseType (IntType (Si (Signed,CInt)))), [$1]}
 | Tfloat               { Right3 (BaseType (FloatType CFloat)),  [$1]}
 | Tdouble              { Right3 (BaseType (FloatType CDouble)), [$1] }
 | Tshort               { Middle3 Short,  [$1]}
 | Tlong                { Middle3 Long,   [$1]}
 | Tsigned              { Left3 Signed,   [$1]}
 | Tunsigned            { Left3 UnSigned, [$1]}
 /*(*c++ext: *)*/
 | Tbool                { Right3 (BaseType (IntType CBool)),            [$1] }
 | Twchar_t             { Right3 (BaseType (IntType WChar_t)),         [$1] }

 /*(* gccext: *)*/
 | Ttypeof TOPar assign_expr TCPar { Right3 (TypeOf ($1,($2,Right $3,$4))),noii}
 | Ttypeof TOPar type_id     TCPar { Right3 (TypeOf ($1,($2,Left $3,$4))),noii}

 /*
 (* history: cant put TIdent {} cos it makes the grammar ambiguous and 
  * generates lots of conflicts => we must use some tricks. 
  * See parsing_hacks_typedef.ml. See also conflicts.txt
  *)*/
 | type_cplusplus_id { Right3 (TypeName $1), noii }


/*(*todo: can have a ::opt nested_name_specifier_opt before ident*)*/
elaborated_type_specifier: 
 | Tenum ident 
     { Right3 (EnumName ($1, $2)), noii }
 | class_key ident
     { Right3 (StructUnionName ($1, $2)), noii }
 /*(* c++ext:  *)*/
 | Ttypename type_cplusplus_id
     { Right3 (TypenameKwd ($1, $2)), noii }

/*(*----------------------------*)*/
/*(*2 c++ext:  *)*/
/*(*----------------------------*)*/

/*(* cant factorize with a tcolcol_opt2 *)*/
type_cplusplus_id:
 | type_name                        { None, noQscope, $1 }
 | nested_name_specifier2 type_name { None, $1, $2 }
 | TColCol_BeforeTypedef type_name  { Some $1, noQscope, $2 }
 | TColCol_BeforeTypedef nested_name_specifier2 type_name 
     { Some $1, $2, $3 }

/*
(* in c++ grammar they put 
 *  typename: enum-name | typedef-name | class-name 
 *  class-name:  identifier | template-id
 *  template-id: template-name < template-argument-list > 
 * 
 * But in my case I don't have the contextual info so when I see an ident
 * it can be a typedef-name, enum-name, or class-name (but not template-name
 * because I detect them as they have a '<' just after),
 * so here type_name is simplified in consequence.
 *)*/
type_name:
 | enum_name_or_typedef_name_or_simple_class_name { IdIdent $1 }
 | template_id { $1 }

template_id:
 | TIdent_Templatename TInf_Template template_argument_list TSup_Template
    { IdTemplateId ($1, ($2, $3, $4)) }

/*
(*c++ext: in the c++ grammar they have also 'template-name' but this is catched 
 * in my case by type_id and its generic TypedefIdent, or will be parsed
 * as an Ident and so assign_expr. In this later case may need a ast post 
 * disambiguation analysis for some false positive.
 *)*/
template_argument:
 | type_id     { Left $1 }
 | assign_expr { Right $1 }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 Qualifiers *)*/
/*(*-----------------------------------------------------------------------*)*/

/*(* was called type_qualif before *)*/
cv_qualif: 
 | Tconst    { {const=Some $1; volatile=None} }
 | Tvolatile { {const=None ; volatile=Some $1} }
 /*(* C99 *)*/
 | Trestrict { (* TODO *) {const=None ; volatile=None} }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 Declarator, right part of a type + second part of decl (the ident)   *)*/
/*(*-----------------------------------------------------------------------*)*/
/*
(* declarator return a couple: 
 *  (name, partial type (a function to be applied to return type))
 *
 * note that with 'int* f(int)' we must return Func(Pointer int,int) and not
 * Pointer (Func(int,int)).
 *)*/

declarator: 
 | pointer direct_d { (fst $2, fun x -> x +> $1 +> (snd $2)  ) }
 | direct_d         { $1  }

/*(* so must do  int * const p; if the pointer is constant, not the pointee *)*/
pointer: 
 | TMul                        { fun x ->(nQ,         (Pointer x,     [$1]))}
 | TMul cv_qualif_list         { fun x ->($2.qualifD, (Pointer x,     [$1]))}
 | TMul pointer                { fun x ->(nQ,         (Pointer ($2 x),[$1]))}
 | TMul cv_qualif_list pointer { fun x ->($2.qualifD, (Pointer ($3 x),[$1]))}
 /*(*c++ext: no qualif for ref *)*/
 | TAnd                        { fun x ->(nQ,    (Reference x,    [$1]))}
 | TAnd pointer                { fun x ->(nQ,    (Reference ($2 x),[$1]))}

direct_d: 
 | declarator_id
     { ($1, fun x -> x) }
 | TOPar declarator TCPar      /*(* forunparser: old: $2 *)*/ 
     { (fst $2, fun x -> (nQ, (ParenType ($1, (snd $2) x, $3), noii))) }
 | direct_d TOCro            TCCro         
     { (fst $1, fun x->(snd $1) (nQ,(Array (($2,None,$3),x), noii))) }
 | direct_d TOCro const_expr TCCro
     { (fst $1, fun x->(snd $1) (nQ,(Array (($2, Some $3, $4),x),noii))) }
 | direct_d TOPar TCPar const_opt exn_spec_opt
     { (fst $1, fun x-> (snd $1) 
         (nQ, (FunctionType {
           ft_ret= x; ft_params = ($2, [], $3);
           ft_dots = None; ft_const = $4; ft_throw = $5; }, noii)))
     }
 | direct_d TOPar parameter_type_list TCPar const_opt exn_spec_opt
     { (fst $1, fun x-> (snd $1) 
          (nQ,(FunctionType { 
            ft_ret = x; ft_params = ($2,fst $3,$4); 
            ft_dots = snd $3; ft_const = $5; ft_throw = $6; }, noii)))
     }

/*(*----------------------------*)*/
/*(*2 c++ext: *)*/
/*(*----------------------------*)*/
declarator_id:
 | tcolcol_opt id_expression 
     { ($1, fst $2, snd $2) }
/*(* TODO ::opt nested-name-specifieropt type-name*) */

/*(*-----------------------------------------------------------------------*)*/
/*(*2 Abstract Declarator (right part of a type, no ident) *)*/
/*(*-----------------------------------------------------------------------*)*/
abstract_declarator: 
 | pointer                            { $1 }
 |         direct_abstract_declarator { $1 }
 | pointer direct_abstract_declarator { fun x -> x +> $2 +> $1 }

direct_abstract_declarator: 
 | TOPar abstract_declarator TCPar /*(* forunparser: old: $2 *)*/
     { (fun x -> (nQ, (ParenType ($1, $2 x, $3), noii))) }
 | TOCro            TCCro                            
     { fun x ->   (nQ, (Array (($1,None, $2), x), noii))}
 | TOCro const_expr TCCro                            
     { fun x ->   (nQ, (Array (($1, Some $2, $3), x), noii))}
 | direct_abstract_declarator TOCro            TCCro 
     { fun x ->$1 (nQ, (Array (($2, None, $3), x), noii)) }
 | direct_abstract_declarator TOCro const_expr TCCro
     { fun x ->$1 (nQ, (Array (($2, Some $3, $4), x), noii)) }
 | TOPar TCPar                                       
     { fun x -> (nQ, (FunctionType {
       ft_ret = x; ft_params = ($1,[],$2); 
       ft_dots = None; ft_const = None; ft_throw = None;}, noii)) }
 | TOPar parameter_type_list TCPar
     { fun x -> (nQ, (FunctionType {
         ft_ret = x; ft_params = ($1,fst $2,$3); 
         ft_dots = snd $2; ft_const = None; ft_throw = None; }, noii)) }
 | direct_abstract_declarator TOPar TCPar const_opt exn_spec_opt
     { fun x -> $1 (nQ, (FunctionType {
         ft_ret = x; ft_params = ($2,[],$3); 
         ft_dots = None; ft_const = $4; ft_throw = $5; }, noii)) }
 | direct_abstract_declarator TOPar parameter_type_list TCPar const_opt 
    exn_spec_opt
     { fun x -> $1 (nQ, (FunctionType {
         ft_ret = x; ft_params = ($2,fst $3,$4); 
         ft_dots = snd $3; ft_const = $5; ft_throw = $6; }, noii)) }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 Parameters (use decl_spec not type_spec just for 'register') *)*/
/*(*-----------------------------------------------------------------------*)*/
parameter_type_list: 
 | parameter_list                  { $1, None }
 | parameter_list TComma TEllipsis { $1, Some ($2,$3) }

parameter_decl: 
 | decl_spec declarator
     { let (t_ret,reg) = type_and_register_from_decl $1 in
       let (name, ftyp) = fixNameForParam $2 in
       { p_name = Some name; p_type = ftyp t_ret;
         p_register = reg; p_val = None } }
 | decl_spec abstract_declarator
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = $2 t_ret; 
         p_register = reg; p_val = None } }
 | decl_spec
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = t_ret; p_register = reg; p_val = None } }

/*(*c++ext: default parameter value, copy paste *)*/
 | decl_spec declarator TEq assign_expr
     { let (t_ret, reg) = type_and_register_from_decl $1 in 
       let (name, ftyp) = fixNameForParam $2 in
       { p_name = Some name; p_type = ftyp t_ret; 
         p_register = reg; p_val = Some ($3, $4) } }
 | decl_spec abstract_declarator TEq assign_expr
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = $2 t_ret; 
         p_register = reg; p_val = Some ($3, $4) } }
 | decl_spec TEq assign_expr
     { let (t_ret, reg) = type_and_register_from_decl $1 in
       { p_name = None; p_type = t_ret; 
         p_register = reg; p_val = Some($2,$3) } }

/*(*----------------------------*)*/
/*(*2 workarounds *)*/
/*(*----------------------------*)*/

parameter_list: 
 | parameter_decl2                       { [$1, []] }
 | parameter_list TComma parameter_decl2 { $1 @ [$3,  [$2]] }

parameter_decl2:
 | parameter_decl { $1 }
 /*(* when the typedef inference didn't work *)*/
 | TIdent
     { 
       let t = nQ, (TypeName (None, [], IdIdent $1), noii) in
       { p_name = None; p_type = t; p_val = None; p_register = None; }
     }

/*(*----------------------------*)*/
/*(*2 c++ext: *)*/
/*(*----------------------------*)*/
/*
(*c++ext: specialisation 
 * TODO should be type-id-listopt. Also they can have qualifiers!
 * need typedef heuristic for throw() but can be also an expression ...
 *)*/
exception_specification: 
 | Tthrow TOPar TCPar { ($1, ($2, [], $3)) }
 | Tthrow TOPar exn_name TCPar { ($1, ($2, [Left $3], $4)) }
 | Tthrow TOPar exn_name TComma exn_name TCPar 
     { ($1, ($2, [Left $3; Right $4; Left $5], $6))  }

exn_name: ident
     { None, [], IdIdent $1 }

/*(*c++ext: in orig they put cv-qualifier-seqopt but it's never volatile so*)*/
const_opt:
 | Tconst        { Some $1 }
 | /*(*empty*)*/ { None }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 helper type rules *)*/
/*(*-----------------------------------------------------------------------*)*/
/*(* For type_id. No storage here. Was used before for field but 
   * now structure fields can have storage so fields now use decl_spec. *)*/
spec_qualif_list: 
 | type_spec                    { addTypeD $1 nullDecl }
 | cv_qualif                    { {nullDecl with qualifD = $1} }
 | type_spec   spec_qualif_list { addTypeD $1 $2   }
 | cv_qualif   spec_qualif_list { addQualifD $1 $2 }

/*(* for pointers in direct_declarator and abstract_declarator *)*/
cv_qualif_list: 
 | cv_qualif                  { {nullDecl with qualifD = $1 } }
 | cv_qualif_list cv_qualif   { addQualifD $2 $1 }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 xxx_type_id *)*/
/*(*-----------------------------------------------------------------------*)*/

/*(* For cast, sizeof, throw. Was called type_name in old C grammar. *)*/
type_id: 
 | spec_qualif_list
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in  t_ret }
 | spec_qualif_list abstract_declarator
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in $2 t_ret }
/*
(* used for the type passed to new(). 
 * There is ambiguity with '*' and '&' cos when have new int *2, it can
 * be parsed as (new int) * 2 or (new int * ) 2.
 * cf p62 of Ellis. So when see a TMul or TAnd don't reduce here,
 * shift, hence the prec when are to decide wether or not to enter
 * in new_declarator and its leading ptr_operator
 *)*/
new_type_id: 
 | spec_qualif_list %prec LOW_PRIORITY_RULE   
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in  t_ret }
 | spec_qualif_list new_declarator 
     { let (t_ret, _, _) = type_and_storage_from_decl $1 in (* TODOAST *) t_ret }

new_declarator: 
 | ptr_operator new_declarator 
     { () }
 | ptr_operator %prec LOW_PRIORITY_RULE
     { () }
 | direct_new_declarator 
     { () }

ptr_operator:
 | TMul { () }
 | TAnd { () }

direct_new_declarator:
 | TOCro expr TCCro { () }
 | direct_new_declarator TOCro expr TCCro { () }

/*
(* in c++ grammar they do 'type_spec_seq conversion_declaratoropt'. We
 * can not replace with a simple 'type_id' cos here we must not allow
 * functionType otherwise there is conflicts on TOPar.
 * TODO: right now do simple_type_specifier because conflict 
 * when do full type_spec.
 *   type_spec conversion_declaratoropt
 * 
 *)*/
conversion_type_id: 
 | simple_type_specifier conversion_declarator 
     { let tx = addTypeD $1 nullDecl in
       let (t_ret, _, _) = type_and_storage_from_decl tx in t_ret 
     }
 | simple_type_specifier %prec LOW_PRIORITY_RULE 
     { let tx = addTypeD $1 nullDecl in
       let (t_ret, _, _) = type_and_storage_from_decl tx in t_ret 
     }

conversion_declarator: 
 | ptr_operator conversion_declarator 
     { () }
 | ptr_operator %prec LOW_PRIORITY_RULE
     { () }

/*(*************************************************************************)*/
/*(*1 Class and struct definitions *)*/
/*(*************************************************************************)*/

/*(* this can come from a simple_declaration/decl_spec *)*/
class_specifier: 
 | class_head TOBrace member_specification_opt TCBrace 
     { let (kind, nameopt, baseopt) = $1 in
       { c_kind = kind; c_name = nameopt; 
         c_inherit = baseopt; c_members = ($2, $3, $4) } }
/*
(* todo in grammar they allow anon class with base_clause, weird. 
 * bugfix_c++: in c++ grammar they put identifier but when we do template
 * specialization then we can get some template_id. Note that can
 * not introduce a class_key_name intermediate cos they get a 
 * r/r conflict as there is another place with a 'class_key ident'
 * in elaborated specifier. So need to duplicate the rule for
 * the template_id case.
*)*/
class_head: 
 | class_key 
     { $1, None, None }
 | class_key ident base_clause_opt
     { let name = None, noQscope, IdIdent $2 in
       $1, Some name, $3 }
 | class_key nested_name_specifier ident base_clause_opt
     { let name = None, $2, IdIdent $3 in
       $1, Some name, $4 }

/*(* was called struct_union before *)*/
class_key: 
 | Tstruct   { Struct, $1 }
 | Tunion    { Union, $1 }
 /*(*c++ext: *)*/
 | Tclass    { Class, $1 }

/*(*----------------------------*)*/
/*(*2 c++ext: inheritance rules *)*/
/*(*----------------------------*)*/
base_clause: 
 | TCol base_specifier_list { $1, $2 }

/*(* base-specifier:
   *  ::opt nested-name-specifieropt class-name
   *  virtual access-specifieropt ::opt nested-name-specifieropt class-name
   *  access-specifier virtualopt ::opt nested-name-specifieropt class-name
   * specialisation
   *)*/
base_specifier:
 | class_name 
     { { i_name = $1; i_virtual = None; i_access = None } }
 | access_specifier class_name 
     { { i_name = $2; i_virtual = None; i_access = Some $1 } }
 | Tvirtual access_specifier class_name 
     { { i_name = $3; i_virtual = Some $1; i_access = Some $2 } }

/*(* TODO? specialisation | ident { $1 }, do heuristic so can remove rule2 *)*/
class_name:
 | type_cplusplus_id  { $1 }
 | TIdent             { None, noQscope, IdIdent $1 }

/*(*----------------------------*)*/
/*(*2 c++ext: members *)*/
/*(*----------------------------*)*/

/*(* todo? add cpp_directive possibility here too *)*/
member_specification:
 | member_declaration member_specification_opt    
     { ClassElem $1::$2 }
 | access_specifier TCol member_specification_opt 
     { ClassElem (Access ($1, $2))::$3 }

access_specifier:
 | Tpublic    { Public, $1 }
 | Tprivate   { Private, $1 }
 | Tprotected { Protected, $1 }


/*(* in c++ grammar there is a ;opt after function_definition but 
   * there is a conflict as it can also be an EmptyField *)*/
member_declaration:
 | field_declaration      { fixFieldOrMethodDecl $1 }
 | function_definition    { MemberFunc (FunctionOrMethod $1) }
 | qualified_id TPtVirg   
     { let name = (None, fst $1, snd $1) in
       QualifiedIdInClass (name, $2)
     }
 | using_declaration      { UsingDeclInClass $1 }
 | template_declaration   { TemplateDeclInClass $1 }

 /*(* not in c++ grammar as merged with function_definition, but I can't *)*/
 | ctor_dtor_member       { $1 }

 /*(* cppext: as some macro sometimes have a trailing ';' we must allow
    * them here. Generates conflicts if keep the opt_ptvirg mentionned 
    * before.
    * c++ext: in c++ grammar they put a double optional but I prefer
    * to force the presence of a decl_spec. I don't know what means
    * 'x;' in a structure, maybe default to int but not practical for my way of
    * parsing
    *)*/
 | TPtVirg    { EmptyField $1 }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 field declaration *)*/
/*(*-----------------------------------------------------------------------*)*/
field_declaration:
 | decl_spec TPtVirg 
     { (* gccext: allow empty elements if it is a structdef or enumdef *)
       let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       let onedecl = { v_namei = None; v_type = t_ret; v_storage = sto } in
       ([(FieldDecl onedecl),noii], $2)
     }
 | decl_spec member_declarator_list TPtVirg 
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       ($2 +> (List.map (fun (f, iivirg) -> f t_ret sto, iivirg)), $3)
     }

/*(* was called struct_declarator before *)*/
member_declarator:
 | declarator                    
     { let (name, partialt) = $1 in (fun t_ret sto -> 
       FieldDecl {
         v_namei = Some (name, None);
         v_type = partialt t_ret; v_storage = sto; })
     }
 /*(* can also be an abstract when it's =0 on a function type *)*/
 | declarator TEq const_expr
     { let (name, partialt) = $1 in (fun t_ret sto -> 
       FieldDecl {
         v_namei = Some (name, Some (EqInit ($2, InitExpr $3)));
         v_type = partialt t_ret; v_storage = sto;
       })
     }

 /*(* normally just ident, but ambiguity so solve by inspetcing declarator *)*/
 | declarator TCol const_expr
     { let (name, _partialt) = fixNameForParam $1 in (fun t_ret _stoTODO -> 
       BitField (Some name, $2, t_ret, $3))
     }
 | TCol const_expr            
     { (fun t_ret _stoTODO -> BitField (None, $1, t_ret, $2)) }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 c++ext: constructor special case *)*/
/*(*-----------------------------------------------------------------------*)*/

/*(* special case for ctor/dtor because they don't have a return type.
   * TODOAST on the ctor_spec and chain of calls
   *)*/
ctor_dtor_member:
 | ctor_spec TIdent_Constructor TOPar parameter_type_list_opt TCPar
     ctor_mem_initializer_list_opt
     compound
     { MemberFunc (Constructor (mk_constructor $2 ($3, $4, $5) $7)) }

 | ctor_spec TIdent_Constructor TOPar parameter_type_list_opt TCPar TPtVirg 
     { MemberDecl (ConstructorDecl ($2, ($3, opt_to_list_params $4, $5), $6)) }

 | dtor_spec TTilde ident TOPar void_opt TCPar exn_spec_opt compound
     { MemberFunc (Destructor (mk_destructor $2 $3 ($4, $5, $6) $7 $8)) }
 | dtor_spec TTilde ident TOPar void_opt TCPar exn_spec_opt TPtVirg
     { MemberDecl (DestructorDecl ($2, $3, ($4, $5, $6), $7, $8)) }


ctor_spec:
 | Texplicit { }
 | Tinline { }
 | /*(*empty*)*/ { }

dtor_spec:
 | Tvirtual { }
 | Tinline { }
 | /*(*empty*)*/ { }

ctor_mem_initializer_list_opt: 
 | TCol mem_initializer_list { () }
 | /*(* empty *)*/ { () }

mem_initializer: 
 | mem_initializer_id TOPar argument_list_opt TCPar { () }

/*(* factorize with declarator_id ? specialisation *)*/
mem_initializer_id:
/* specialsiation | TIdent { () } */
 | primary_cplusplus_id { () }

/*(*************************************************************************)*/
/*(*1 Enum definition *)*/
/*(*************************************************************************)*/

enum_specifier: 
 | Tenum        TOBrace enumerator_list gcc_comma_opt TCBrace
     { EnumDef ($1, None, ($2, $3, $5)) (*$4*) }
 | Tenum ident  TOBrace enumerator_list gcc_comma_opt TCBrace
     { EnumDef ($1, Some $2, ($3, $4, $6)) (*$5*) }

enumerator: 
 | ident                { { e_name = $1; e_val = None; } }
 | ident TEq const_expr { { e_name = $1; e_val = Some ($2, $3); } }

/*(*************************************************************************)*/
/*(*1 Simple declaration, initializers *)*/
/*(*************************************************************************)*/

simple_declaration:
 | decl_spec TPtVirg
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in 
       DeclList ([{v_namei = None; v_type = t_ret; v_storage = sto},noii],$2)
     }
 | decl_spec init_declarator_list TPtVirg 
     { let (t_ret, sto, _inline) = type_and_storage_from_decl $1 in
       DeclList (
         ($2 +> List.map (fun (((name, f), iniopt), iivirg) ->
           (* old: if fst (unwrap storage)=StoTypedef then LP.add_typedef s; *)
           { v_namei = Some (name, iniopt);
             v_type = f t_ret; v_storage = sto
           },
           iivirg
         )), $3)
     } 
 /*(* cppext: *)*/
 | TIdent_MacroDecl TOPar argument_list TCPar TPtVirg 
     { MacroDecl ([], $1, ($2, $3, $4), $5) }
 | Tstatic TIdent_MacroDecl TOPar argument_list TCPar TPtVirg 
     { MacroDecl ([$1], $2, ($3, $4, $5), $6) }
 | Tstatic Tconst_MacroDeclConst 
    TIdent_MacroDecl TOPar argument_list TCPar TPtVirg 
     { MacroDecl ([$1;$2], $3, ($4, $5, $6), $7) }

/*(*-----------------------------------------------------------------------*)*/
/*
(* In c++ grammar they put 'explicit' in function_spec, 'typedef' and 'friend' 
 * in decl_spec. But it's just estethic as no other rules directly
 * mention function_spec or storage_spec. They just want to say that 
 * 'virtual' applies only to functions, but they have no way to check that 
 * syntaxically. I could keep as before, as in the C grammar. 
 * For 'explicit' I prefer to put it directly
 * with the ctor as I already have a special heuristic for constructor.
 * They also don't put the cv_qualif here but instead inline it in
 * type_spec. I prefer to keep as before but I take care when
 * they speak about type_spec to translate instead in type+qualif_spec
 * (which is spec_qualif_list)
 * 
 * todo? can simplify by putting all in _opt ? must have at least one otherwise
 * decl_list is ambiguous ? (no cos have ';' between decl) 
 *)*/
decl_spec: 
 | storage_class_spec      { {nullDecl with storageD = $1 } }
 | type_spec               { addTypeD $1 nullDecl }
 | cv_qualif               { {nullDecl with qualifD  = $1 } }
 | function_spec           { {nullDecl with inlineD = (true, [snd $1]) } (*TODO*) }
 | Ttypedef     { {nullDecl with storageD = StoTypedef $1 } }
 | Tfriend      { {nullDecl with inlineD = (true, [$1]) } (*TODO*) }

 | storage_class_spec decl_spec { addStorageD $1 $2 }
 | type_spec          decl_spec { addTypeD  $1 $2 }
 | cv_qualif          decl_spec { addQualifD $1 $2 }
 | function_spec      decl_spec { addInlineD (snd $1) $2 (*TODO*) }
 | Ttypedef           decl_spec { addStorageD (StoTypedef $1) $2 }
 | Tfriend            decl_spec { addInlineD $1 $2 (*TODO*)}

function_spec:
 /*(*gccext: and c++ext: *)*/
 | Tinline { Inline, $1 }
 /*(*c++ext: *)*/
 | Tvirtual { Virtual, $1 }

storage_class_spec: 
 | Tstatic      { Sto (Static,  $1) }
 | Textern      { Sto (Extern,  $1) }
 | Tauto        { Sto (Auto,    $1) }
 | Tregister    { Sto (Register,$1) }
 /*(* c++ext: *)*/
 | Tmutable     { Sto (Register,$1) (*TODO*) }

/*(*-----------------------------------------------------------------------*)*/
/*(*2 declarators (right part of type and variable) *)*/
/*(*-----------------------------------------------------------------------*)*/
init_declarator:  
 | declaratori                  { ($1, None) }
 | declaratori TEq initialize   { ($1, Some (EqInit ($2, $3))) }
 /*
 (* c++ext: c++ initializer via call to constructor. Note that this
  * is different from TypedefIdent2, here the declaratori is an ident,
  * not the constructorname hence the need for a TOPar_CplusplusInit
  *)*/
 | declaratori TOPar_CplusplusInit argument_list_opt TCPar 
     { ($1, Some (ObjInit ($2, $3, $4))) }

/*(*----------------------------*)*/
/*(*2 gccext: *)*/
/*(*----------------------------*)*/
declaratori: 
 | declarator                { $1 }
 /*(* gccext: *)*/ 
 | declarator gcc_asm_decl   { $1 }

gcc_asm_decl: 
 | Tasm volatile_opt TOPar asmbody TCPar        {  }
			  
/*(*-----------------------------------------------------------------------*)*/
/*(*2 initializers *)*/
/*(*-----------------------------------------------------------------------*)*/
initialize: 
 | assign_expr                                    
     { InitExpr $1 }
 | TOBrace initialize_list gcc_comma_opt_struct  TCBrace
     { InitList ($1, List.rev $2, $4) (*$3*) }
 /*(* gccext: *)*/
 | TOBrace TCBrace
     { InitList ($1, [], $2) } 

/*
(* opti: This time we use the weird order of non-terminal which requires in 
 * the "caller" to do a List.rev cos quite critical. With this wierd order it
 * allows yacc to use a constant stack space instead of exploding if we would
 * do a  'initialize2 Tcomma initialize_list'.
 *)
*/
initialize_list: 
 | initialize2                        { [$1,   []] }
 | initialize_list TComma initialize2 { ($3,  [$2])::$1 }


/*(* gccext: condexpr and no assign_expr cos can have ambiguity with comma *)*/
initialize2: 
 | cond_expr 
     { InitExpr $1 } 
 | TOBrace initialize_list gcc_comma_opt_struct TCBrace
     { InitList ($1, List.rev $2, $4) (*$3*) }
 | TOBrace TCBrace
     { InitList ($1, [], $2) }

 /*(* gccext: labeled elements, a.k.a designators *)*/
 | designator_list TEq initialize2 
     { InitDesignators ($1, $2, $3) }
 /*(* gccext: old format, in old kernel for instance *)*/
 | ident TCol initialize2
     { InitFieldOld ($1, $2, $3) }

/*(* kenccext: c++ext:, but conflcit with array designators *)*/
 | TOCro const_expr TCCro initialize2
     { InitIndexOld (($1, $2, $3), $4) }
 | TOCro const_expr TCCro  TEq initialize2
     { InitDesignators ([DesignatorIndex($1, $2, $3)], $4, $5) }

/*(* they can be nested, can have a .x.[3].y *)*/
designator: 
 | TDot ident 
     { DesignatorField ($1, $2) } 
/* conflict with kenccext
 | TOCro const_expr TCCro     %prec LOW_PRIORITY_RULE
     { DesignatorIndex ($1, $2, $3) }
 | TOCro const_expr TEllipsis const_expr TCCro 
     { DesignatorRange ($1, ($2, $3, $4), $5) }
*/
/*(*----------------------------*)*/
/*(*2 workarounds *)*/
/*(*----------------------------*)*/
gcc_comma_opt_struct: 
 | TComma           { Some $1 } 
 | /*(* empty *)*/  { None  }

/*(*************************************************************************)*/
/*(*1 Block declaration (namespace and asm) *)*/
/*(*************************************************************************)*/

block_declaration:
 | simple_declaration { $1 }

 /*(*gccext: *)*/
 | asm_definition     { $1 }
 /*(*c++ext: *)*/
 | namespace_alias_definition { $1 }
 | using_declaration { UsingDecl $1 }
 | using_directive   { $1 }


/*(*----------------------------*)*/
/*(*2 c++ext: *)*/
/*(*----------------------------*)*/

namespace_alias_definition:
 | Tnamespace TIdent TEq tcolcol_opt nested_name_specifier_opt namespace_name
    TPtVirg
     { let name = $4, $5, IdIdent $6 in NameSpaceAlias ($1, $2, $3, name, $7) }

using_directive:
 | Tusing Tnamespace tcolcol_opt nested_name_specifier_opt namespace_name 
    TPtVirg
     { let name = $3, $4, IdIdent $5 in UsingDirective ($1, $2, name, $6) }

/*(* conflict on TColCol in 'Tusing TColCol unqualified_id TPtVirg'
   * need LALR(2) to see if after tcol have a nested_name_specifier
   * or put opt on nested_name_specifier too
  *)*/
using_declaration:
 | Tusing typename_opt tcolcol_opt nested_name_specifier unqualified_id TPtVirg
     { let name = ($3, $4, $5) in $1, name, $6 (*$2*) }
/*(* TODO: remove once we don't skip qualifier ? *)*/
 | Tusing typename_opt tcolcol_opt unqualified_id TPtVirg 
     { let name = ($3, [], $4) in $1, name, $5 (*$2*) }
  
/*(*----------------------------*)*/
/*(*2 gccext: c++ext: *)*/
/*(*----------------------------*)*/

/*(* gccext: c++ext: also apparently *)*/
asm_definition:
 | Tasm volatile_opt TOPar asmbody TCPar TPtVirg           
     { Asm($1, $2, ($3, $4, $5), $6) }

asmbody: 
 | string_list colon_asm_list  { $1, $2 }
 | string_list { $1, [] } /*(* in old kernel *)*/

colon_asm: 
 | TCol colon_option_list { Colon $2, [$1]   }

colon_option: 
 | TString                      { ColonMisc, [snd $1] }
 | TString TOPar asm_expr TCPar { ColonExpr ($2, $3, $4), [snd $1] } 
 /*(* cppext: certainly a macro *)*/
 | TOCro TIdent TCCro TString TOPar asm_expr TCPar
     { ColonExpr ($5, $6, $7), [$1;snd $2;$3;snd $4] }
 | TIdent                           { ColonMisc, [snd $1] }
 | /*(* empty *)*/                  { ColonMisc, [] }

asm_expr: assign_expr { $1 }

/*(*************************************************************************)*/
/*(*1 Declaration, in c++ sense *)*/
/*(*************************************************************************)*/
/*
(* in grammar they have 'explicit_instantiation' but it is equal to 
 * to template_declaration and so is ambiguous.
 * 
 * declaration > block_declaration > simple_declaration, hmmm
 * could be renamed declaration_or_definition
 *)*/
declaration:
 | block_declaration                 { BlockDecl $1 }

 | function_definition               { Func (FunctionOrMethod $1) }

 /*(* not in c++ grammar as merged with function_definition, but I can't *)*/
 | ctor_dtor { $1 }
 | template_declaration              { let (a,b,c) = $1 in TemplateDecl (a,b,c)}
 | explicit_specialization           { $1 }
 | linkage_specification             { $1 }
 | namespace_definition              { $1 }

 /*(* sometimes the function ends with }; instead of just } *)*/
 | TPtVirg    { EmptyDef $1 } 


/*(*----------------------------*)*/
/*(*2 cppext: *)*/
/*(*----------------------------*)*/

declaration_list_opt: 
 | /*(*empty*)*/ { [] }
 | declaration_list { $1 }

declaration_list: 
 | declaration_seq                  { [$1]   }
 | declaration_list declaration_seq { $1 @ [$2] }

declaration_seq:
 | declaration { DeclElem $1 }
 /* (* cppext: *)*/
 | cpp_directive 
     { CppDirectiveDecl $1 }
 | cpp_ifdef_directive/*(* stat_or_decl_list ...*)*/  
     { IfdefDecl $1 }

/*(*----------------------------*)*/
/*(*2 c++ext: *)*/
/*(*----------------------------*)*/

/*(*todo: export_opt, but generates lots of conflicts *)*/ 
template_declaration:
 | Ttemplate TInf_Template template_parameter_list TSup_Template declaration
   { ($1, ($2, $3, $4), $5) }

explicit_specialization:
 | Ttemplate TInf_Template TSup_Template declaration 
     { TemplateSpecialization ($1, ($2, (), $3), $4) }

/*(*todo: '| type_paramter' 
   * ambiguity with parameter_decl cos a type can also be 'class X'
 | Tclass ident { raise Todo }
   *)*/
template_parameter: 
 | parameter_decl { $1 }


/*(* c++ext: could also do a extern_string_opt to factorize stuff *)*/
linkage_specification:
 | Textern TString declaration 
     { ExternC ($1, (snd $2), $3) }
 | Textern TString TOBrace declaration_list_opt TCBrace 
     { ExternCList ($1, (snd $2), ($3, $4, $5)) }


namespace_definition:
 | named_namespace_definition   { $1 }
 | unnamed_namespace_definition { $1 }

/*
(* in c++ grammar they make diff between 'original' and 'extension' namespace
 * definition but they require some contextual information to know if
 * an identifier was already a namespace. So here I have just a single rule.
 *)*/
named_namespace_definition: 
 | Tnamespace TIdent TOBrace declaration_list_opt TCBrace 
     { NameSpace ($1, $2, ($3, $4, $5)) }

unnamed_namespace_definition:
 | Tnamespace TOBrace declaration_list_opt TCBrace 
     { NameSpaceAnon ($1, ($2, $3, $4)) }


/*
(* Special case cos ctor/dtor do not have return type. 
 * TODO scope ? do a start_constructor ? 
 *)*/
ctor_dtor:
 | nested_name_specifier TIdent_Constructor TOPar parameter_type_list_opt TCPar
     ctor_mem_initializer_list_opt
     compound
     { DeclTodo }
 /*(* new_type_id, could also introduce a Tdestructorname or forbidy the
      TypedefIdent2 transfo by putting a guard in the lalr(k) rule by
      checking if have a ~ before
   *)*/
 | nested_name_specifier TTilde ident TOPar void_opt TCPar compound
     { DeclTodo }

/*(* TODO: remove once we don't skip qualifiers *)*/
 | inline_opt TIdent_Constructor TOPar parameter_type_list_opt TCPar
     ctor_mem_initializer_list_opt
     compound
     { DeclTodo }
 | TTilde ident TOPar void_opt TCPar exn_spec_opt compound
     { DeclTodo }

/*(*************************************************************************)*/
/*(*1 Function definition *)*/
/*(*************************************************************************)*/

function_definition: start_fun compound 
     { fixFunc ($1, $2) }

start_fun: decl_spec declarator
     { let (t_ret, sto) = type_and_storage_for_funcdef_from_decl $1 in
       (fst $2, fixOldCDecl ((snd $2) t_ret), sto)
     }

/*(*************************************************************************)*/
/*(*1 Cpp directives *)*/
/*(*************************************************************************)*/

/*(* cppext: *)*/
cpp_directive: 
 | TInclude 
     { let (_include_str, filename, tok) = $1 in
       (* redo some lexing work :( *)
       let inc_kind, path = 
         match () with
         | _ when filename =~ "^\"\\(.*\\)\"$" ->  Local, matched1 filename
         | _ when filename =~ "^\\<\\(.*\\)\\>$" -> Standard, matched1 filename
         | _ -> Weird, filename
       in
       Include (tok, inc_kind, path)
     }

 | TDefine TIdent_Define define_val TCommentNewline_DefineEndOfMacro
     { Define ($1, $2, DefineVar, $3) (*$4??*) }
 /*
 (* The TOPar_Define is introduced to avoid ambiguity with previous rules.
  * A TOPar_Define is a TOPar that was just next to the ident (no space).
  * See parsing_hacks_define.ml
  *)*/
 | TDefine TIdent_Define TOPar_Define param_define_list_opt TCPar 
    define_val TCommentNewline_DefineEndOfMacro
     { Define ($1, $2, (DefineFunc ($3, $4, $5)), $6) (*$7*) }

 | TUndef             { Undef $1 }
 | TCppDirectiveOther { PragmaAndCo $1 }

define_val: 
 /*(* perhaps better to use assign_expr? but in that case need 
    * do a assign_expr_of_string'in parse_c.
    * c++ext: update, now statement include simple declarations
    * so maybe can parse $1 and generate the previous DefineDecl
    * and DefineFunction? cos nested_func is also now inside statement.
    *)*/
 | expr      { DefineExpr $1 }
 | statement { DefineStmt $1 }
 /*(* for statement-like macro with fixed number of arguments *)*/
 | Tdo statement Twhile TOPar expr TCPar 
     { match $5 with
       | (C (Int ("0"))), [tok] -> DefineDoWhileZero ($2,  [$1;$3;$4;tok;$6])
       | _ -> raise Parsing.Parse_error
     }
 /*(* for statement-like macro with varargs *)*/
 | Tif TOPar expr TCPar id_expression
     { let name = (None, fst $5, snd $5) in 
       DefinePrintWrapper ($1, ($2, $3, $4), name) 
     }
 | TOBrace_DefineInit initialize_list TCBrace comma_opt 
    { DefineInit (InitList ($1, List.rev $2, $3) (*$4*))  }
 | /*(* empty *)*/ { DefineEmpty }


param_define:
 | ident               { fst $1, [snd $1] }

 | TDefParamVariadic    { fst $1, [snd $1] } 
 | TEllipsis            { "...", [$1] }
 /*(* they reuse keywords :(  *)*/
 | Tregister            { "register", [$1] }
 | Tnew                 { "new", [$1] }


cpp_ifdef_directive: 
 | TIfdef     { Ifdef, $1 }
 | TIfdefelse { IfdefElse, $1 }
 | TIfdefelif { IfdefElseif, $1 }
 | TEndif     { IfdefEndif, $1 }

 | TIfdefBool  { Ifdef, snd $1 }
 | TIfdefMisc  { Ifdef, snd $1 }
 | TIfdefVersion { Ifdef, snd $1 }

cpp_other:
/*(* cppext: *)*/
 | TIdent TOPar argument_list TCPar TPtVirg
     { MacroTop ($1, ($2, $3, $4), Some $5) } 

 /*(* TCPar_EOL to fix the end-of-stream bug of ocamlyacc *)*/
 | TIdent TOPar argument_list TCPar_EOL
     { MacroTop ($1, ($2, $3, $4), None) } 

  /*(* ex: EXPORT_NO_SYMBOLS; *)*/
 | TIdent TPtVirg { MacroVarTop ($1, $2) }

/*(*************************************************************************)*/
/*(*1 toplevel *)*/
/*(*************************************************************************)*/

toplevel: 
 | toplevel_aux { Some $1 }
 | EOF          { None }

toplevel_aux:
 | declaration         { DeclElem $1 }

 | cpp_directive       { CppDirectiveDecl $1 }
 | cpp_ifdef_directive /*(*external_declaration_list ...*)*/ { IfdefDecl $1 }
 | cpp_other           { $1 }

 /*
 (* when have error recovery, we can end up skipping the
  * beginning of the file, and so get trailing unclose } at
  * end
  *)*/
 | TCBrace { DeclElem (EmptyDef $1) }

/*(*************************************************************************)*/
/*(*1 xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/

string_list: 
 | string_elem { $1 }
 | string_list string_elem { $1 @ $2 } 

colon_asm_list: 
 | colon_asm { [$1] }
 | colon_asm_list colon_asm  { $1 @ [$2] }

colon_option_list: 
 | colon_option { [$1, []] } 
 | colon_option_list TComma colon_option { $1 @ [$3, [$2]] }


argument_list: 
 | argument                           { [$1, []] }
 | argument_list TComma argument { $1 @ [$3,    [$2]] }


enumerator_list: 
 | enumerator                        { [$1,          []]   }
 | enumerator_list TComma enumerator { $1 @ [$3,    [$2]] }


init_declarator_list: 
 | init_declarator                             { [$1,   []] }
 | init_declarator_list TComma init_declarator { $1 @ [$3,     [$2]] }

member_declarator_list: 
 | member_declarator                             { [$1,   []] }
 | member_declarator_list TComma member_declarator { $1 @ [$3,     [$2]] }




param_define_list_opt: 
 | /*(* empty *)*/ { [] }
 | param_define                           { [$1, []] }
 | param_define_list_opt TComma param_define  { $1 @ [$3, [$2]] }

designator_list: 
 | designator { [$1] }
 | designator_list designator { $1 @ [$2] }


handler_list:
 | handler { [$1] }
 | handler_list handler { $1 @ [$2] }


mem_initializer_list: 
 | mem_initializer                           { [$1, []] }
 | mem_initializer_list TComma mem_initializer  { $1 @ [$3, [$2]] }

template_argument_list:
 | template_argument { [$1, []] }
 | template_argument_list TComma template_argument { $1 @ [$3, [$2]] }

template_parameter_list: 
 | template_parameter { [$1, []] }
 | template_parameter_list TComma template_parameter { $1 @ [$3, [$2]] }


base_specifier_list: 
 | base_specifier                               { [$1,           []] }
 | base_specifier_list TComma base_specifier    { $1 @ [$3,     [$2]] }

/*(*-----------------------------------------------------------------------*)*/

/*(* gccext:  which allow a trailing ',' in enum, as in perl *)*/
gcc_comma_opt: 
 | TComma {  [$1] } 
 | /*(* empty *)*/  {  []  }

comma_opt:
 | TComma { [$1] }
 | { [] }


assign_expr_opt: 
 | assign_expr     { Some $1 }
 | /*(* empty *)*/ { None }

expr_opt: 
 | expr            { Some $1 }
 | /*(* empty *)*/ { None }



argument_list_opt:
 | argument_list { $1 }
 | /*(*empty*)*/ { [] }

parameter_type_list_opt:
 | parameter_type_list { Some $1 }
 | /*(*empty*)*/       { None }


member_specification_opt:
 | member_specification { $1 }
 | /*(*empty*)*/        { [] }


nested_name_specifier_opt:
 | nested_name_specifier { $1 }
 | /*(* empty *)*/       { [] }

nested_name_specifier_opt2:
 | nested_name_specifier2 { $1 }
 | /*(* empty *)*/        { [] }



exn_spec_opt: 
 | exception_specification { Some $1 }
 | /*(*empty*)*/           { None }


/*(*c++ext: ??? *)*/
new_placement_opt:
 | new_placement { Some $1 }
 | /*(*empty*)*/ { None }

new_initializer_opt:
 | new_initializer { Some $1 }
 | /*(*empty*)*/ { None }



base_clause_opt: 
 | base_clause   { Some $1 }
 | /*(*empty*)*/ { None }

typename_opt:
 | Ttypename     { [$1] }
 | /*(*empty*)*/ { [] }

template_opt:
 | Ttemplate     { [$1] }
 | /*(*empty*)*/ { [] }

/*
export_opt:
 | Texport   { Some $1 }
 | (*empty*) { None }

ptvirg_opt:
 | TPtVirg { [$1] }
 | { [] }
*/

void_opt:
 | Tvoid         { Some $1 }
 | /*(*empty*)*/ { None }

inline_opt:
 | Tinline         { Some $1 }
 | /*(*empty*)*/ { None }

volatile_opt:
 | Tvolatile     { Some $1 }
 | /*(*empty*)*/ { None }

tcolcol_opt:
 | TColCol         { Some $1 }
 | /*(* empty *)*/ { None }
