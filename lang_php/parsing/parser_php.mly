/*(*s: parser_php.mly *)*/
/*(*s: Facebook copyright2 *)*/
/* Yoann Padioleau
 * 
 * Copyright (C) 2009-2012 Facebook
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
/*(*e: Facebook copyright2 *)*/

/*(*s: GRAMMAR prelude *)*/
%{
(* src: ocamlyaccified from zend_language_parser.y in PHP source code.
 * update: 
 *  - extended to deal with XHP based on XHP bison grammar.
 *  - added support for a few PHP 5.3 extensions (e.g. lambda, const, but
 *    not namespace)
 *  - added support for yield (facebook extension)
 *  - added support for a few PHP 5.4 extensions (e.g. traits)
 * 
 /*(*s: Zend copyright *)*/
  * +----------------------------------------------------------------------+
  * | Zend Engine                                                          |
  * +----------------------------------------------------------------------+
  * | Copyright (c) 1998-2006 Zend Technologies Ltd. (http://www.zend.com) |
  * +----------------------------------------------------------------------+
  * | This source file is subject to version 2.00 of the Zend license,     |
  * | that is bundled with this package in the file LICENSE, and is        |
  * | available through the world-wide-web at the following url:           |
  * | http://www.zend.com/license/2_00.txt.                                |
  * | If you did not receive a copy of the Zend license and are unable to  |
  * | obtain it through the world-wide-web, please send a note to          |
  * | license@zend.com so we can mail you a copy immediately.              |
  * +----------------------------------------------------------------------+
  * | Authors: Andi Gutmans <andi@zend.com>                                |
  * |          Zeev Suraski <zeev@zend.com>                                |
  * +----------------------------------------------------------------------+
 /*(*e: Zend copyright *)*/
 * 
 * /* Id: zend_language_parser.y 263383 2008-07-24 11:47:14Z dmitry */
 * 
 * LALR shift/reduce conflicts and how they are resolved:
 *
 *  - 2 shift/reduce conflicts due to the dangeling elseif/else ambiguity.  
 *  Solved by shift.
 * 
 * %pure_parser
 * %expect 2
 * 
 *)
open Common

open Ast_php
open Parser_php_mly_helper

module Ast = Ast_php
%}

/*(*e: GRAMMAR prelude *)*/
/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR tokens declaration *)*/

%token <Ast_php.info> TUnknown /*(* unrecognized token *)*/
%token <Ast_php.info> EOF

/*(*-----------------------------------------*)*/
/*(*2 The space/comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(*s: GRAMMAR comment tokens *)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Ast_php.info> TSpaces TNewline

/*(* not mentionned in this grammar. filtered in parse_php.ml *)*/
%token <Ast_php.info> T_COMMENT T_DOC_COMMENT

/*(* when use preprocessor and want to mark removed tokens as commented *)*/
%token <Ast_php.info> TCommentPP
/*(*e: GRAMMAR comment tokens *)*/

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/
/*(*s: GRAMMAR normal tokens *)*/
%token <string * Ast_php.info> 
 T_LNUMBER T_DNUMBER
 /*(* T_IDENT is for a regular ident and  T_VARIABLE is for a dollar ident.
   * Note that with XHP if you want to add a rule using T_IDENT, you should
   * probably use 'ident' instead.
   *)*/
 T_IDENT T_VARIABLE
 T_CONSTANT_ENCAPSED_STRING   T_ENCAPSED_AND_WHITESPACE  T_INLINE_HTML
 /*(* used only for offset of array access inside strings *)*/
 T_NUM_STRING
 T_STRING_VARNAME
/*(*in original: %token <Ast_php.info> T_CHARACTER T_BAD_CHARACTER *)*/

/*(*-----------------------------------------*)*/
/*(*2 Keyword tokens *)*/
/*(*-----------------------------------------*)*/

%token <Ast_php.info> 
 T_IF T_ELSE T_ELSEIF T_ENDIF
 T_DO  T_WHILE   T_ENDWHILE  T_FOR     T_ENDFOR T_FOREACH T_ENDFOREACH
 T_SWITCH  T_ENDSWITCH T_CASE T_DEFAULT    T_BREAK T_CONTINUE
 T_RETURN  T_TRY  T_CATCH T_THROW
 T_EXIT T_DECLARE T_ENDDECLARE T_USE T_GLOBAL T_AS T_FUNCTION T_CONST T_VAR
/*(* ugly: because of my hack around the implicit echo when use <?=, 
   * this T_ECHO might have a string different than "echo"
   *)*/
 T_ECHO  T_PRINT
 /*(* pad: was declared via right ... ??? mean token ? *)*/
 T_STATIC  T_ABSTRACT  T_FINAL  T_PRIVATE T_PROTECTED T_PUBLIC
 T_UNSET T_ISSET T_EMPTY
 T_CLASS   T_INTERFACE  T_EXTENDS T_IMPLEMENTS
 T_TRAIT T_INSTEADOF
 T_LIST T_ARRAY
 T_CLASS_C T_METHOD_C T_FUNC_C T_LINE   T_FILE T_DIR T_TRAIT_C
 T_LOGICAL_OR   T_LOGICAL_AND   T_LOGICAL_XOR
 T_NEW T_CLONE T_INSTANCEOF
 T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE
 T_EVAL 
 /*(* not in original grammar *)*/
 T_SELF T_PARENT

/*(*-----------------------------------------*)*/
/*(*2 Symbol tokens *)*/
/*(*-----------------------------------------*)*/

%token <Ast_php.info> 
 T_OBJECT_OPERATOR T_DOUBLE_ARROW
 T_OPEN_TAG  T_CLOSE_TAG T_OPEN_TAG_WITH_ECHO T_CLOSE_TAG_OF_ECHO
 T_START_HEREDOC    T_END_HEREDOC
 T_DOLLAR_OPEN_CURLY_BRACES T_CURLY_OPEN
 TCOLCOL
 /*(* pad: was declared as left/right, without a token decl in orig gram *)*/
 TCOLON TCOMMA TDOT TBANG TTILDE TQUESTION
 TOBRA 
 TPLUS TMINUS TMUL TDIV TMOD
 TAND TOR TXOR
 TEQ TSMALLER TGREATER
 T_PLUS_EQUAL  T_MINUS_EQUAL  T_MUL_EQUAL  T_DIV_EQUAL
 T_CONCAT_EQUAL  T_MOD_EQUAL 
 T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
 T_INC    T_DEC
 T_BOOLEAN_OR   T_BOOLEAN_AND 
 T_SL    T_SR
 T_IS_SMALLER_OR_EQUAL    T_IS_GREATER_OR_EQUAL
 T_BOOL_CAST T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST 
 T_UNSET_CAST
 T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_EQUAL     T_IS_NOT_EQUAL
 T__AT
 /*(* was declared implicitely because was using directly the character *)*/
 TOPAR TCPAR  TOBRACE TCBRACE
 TCBRA TBACKQUOTE
/*(* ugly: because of my hack around the implicit ';' when use ?>, 
   * this TSEMICOLON might have a string different than ';'
   *)*/
 TSEMICOLON
 TDOLLAR /*(* see also T_VARIABLE *)*/
 TGUIL
/*(*e: GRAMMAR normal tokens *)*/

/*(*-----------------------------------------*)*/
/*(*2 Extra tokens: *)*/
/*(*-----------------------------------------*)*/
/*(*s: GRAMMAR tokens hook *)*/
%token <Ast_php.info> TDOTS
/*(*x: GRAMMAR tokens hook *)*/

/*(*x: GRAMMAR tokens hook *)*/
%token <Ast_php.info> T_CLASS_XDEBUG T_RESOURCE_XDEBUG
/*(*e: GRAMMAR tokens hook *)*/

/*(*-----------------------------------------*)*/
/*(*2 PHP language extensions: *)*/
/*(*-----------------------------------------*)*/
%token <Ast_php.info> T_YIELD

/*(*-----------------------------------------*)*/
/*(*2 XHP tokens *)*/
/*(*-----------------------------------------*)*/

/*(* xhp: token for ':frag:foo' for instance; quite similiar to T_IDENT *)*/
%token <string list * Ast_php.info> T_XHP_COLONID_DEF
/*(* xhp: token for '%frag:foo' *)*/
%token <string list * Ast_php.info> T_XHP_PERCENTID_DEF

/*(* xhp: e.g. for '<x:frag', note that the real end of the tag is
   * in another token, either T_XHP_GT or T_XHP_SLASH_GT.
   *)*/
%token <Ast_php.xhp_tag * Ast_php.info> T_XHP_OPEN_TAG

/*(* ending part of the opening tag *)*/
%token <Ast_php.info> T_XHP_GT T_XHP_SLASH_GT

/*(* xhp: e.g. for '</x:frag>'. The 'option' is for closing tags like </> *)*/
%token <Ast_php.xhp_tag option * Ast_php.info> T_XHP_CLOSE_TAG

%token <string * Ast_php.info> T_XHP_ATTR T_XHP_TEXT

/*(* xhp keywords. If you add one don't forget to update the 'ident' rule. *)*/
%token <Ast_php.info> 
 T_XHP_ATTRIBUTE T_XHP_CHILDREN T_XHP_CATEGORY
 T_XHP_ENUM T_XHP_REQUIRED
 T_XHP_ANY /*(* T_XHP_EMPTY is T_EMPTY *)*/
 T_XHP_PCDATA

/*(*e: GRAMMAR tokens declaration *)*/

/*(*s: GRAMMAR tokens priorities *)*/
/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc SHIFTHERE

%left      T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE
%left      TCOMMA
%left      T_LOGICAL_OR
%left      T_LOGICAL_XOR
%left      T_LOGICAL_AND
%right     T_PRINT
%left      TEQ T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
%left      TQUESTION TCOLON
%left      T_BOOLEAN_OR
%left      T_BOOLEAN_AND
%left      TOR
%left      TXOR
%left      TAND
%nonassoc  T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL
%nonassoc  TSMALLER T_IS_SMALLER_OR_EQUAL TGREATER T_IS_GREATER_OR_EQUAL
%left      T_SL T_SR
%left      TPLUS TMINUS TDOT
%left      TMUL TDIV TMOD
%right     TBANG
%nonassoc  T_INSTANCEOF
%right     TTILDE T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST 
%right     T__AT
%right     TOBRA
%nonassoc  T_NEW T_CLONE
%left      T_ELSEIF
%left      T_ELSE
%left      T_ENDIF
%nonassoc  T_YIELD

/*(* xhp: this is used only to remove some shift/reduce ambiguities on the
   * error-rule trick.
   *)*/
%left T_XHP_PERCENTID_DEF

/*(*e: GRAMMAR tokens priorities *)*/
/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/
%start main expr class_declaration_statement sgrep_spatch_pattern
/*(*s: GRAMMAR type of main rule *)*/
%type <Ast_php.toplevel list> main
%type <Ast_php.expr> expr
%type <Ast_php.class_def> class_declaration_statement
%type <Ast_php.any> sgrep_spatch_pattern
/*(*e: GRAMMAR type of main rule *)*/

%%

/*(*s: GRAMMAR long set of rules *)*/
/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR toplevel *)*/
main: start EOF { squash_stmt_list $1 ++ [FinalDef $2] }

start: top_statement_list { $1 }

/*(*x: GRAMMAR toplevel *)*/
top_statement:
 | statement                            { StmtList [$1] }
 | constant_declaration_statement       { ConstantDef $1 }
 | function_declaration_statement	{ FuncDef $1 }
 | class_declaration_statement		{ ClassDef $1 }
 | trait_declaration_statement          { ClassDef $1 }

/*(*e: GRAMMAR toplevel *)*/
sgrep_spatch_pattern:
 | expr EOF      { Expr $1 }
 | statement EOF { Stmt2 $1 }
 | statement statement EOF { Stmt2 $1 }
 | function_declaration_statement { Toplevel (FuncDef $1) }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR statement *)*/
inner_statement:
 | statement                            { Stmt $1 }
 | function_declaration_statement	{ FuncDefNested $1 }
 | class_declaration_statement		{ ClassDefNested $1 }

statement: unticked_statement { $1 }
/*(*x: GRAMMAR statement *)*/
unticked_statement:
 | expr           TSEMICOLON		  { ExprStmt($1,$2) }
 | /*(* empty*)*/ TSEMICOLON              { EmptyStmt($1) }

  /*(* static-php-ext: *)*/
/*
(* todo: this is commented because it is not really used
 * and it generates some conflicts now that type_hint
 * is not anymore   type_hint: ident { ... } but 
 * type_hint: class_name_or_selfparent { ... }
 * 
 * | type_hint variable TSEMICOLON          { 
 * if not !Flag_parsing_php.type_hints_extension
 * then raise Parsing.Parse_error;
 * TypedDeclaration ($1, $2, None, $3)
 * }
 * | type_hint variable TEQ expr TSEMICOLON { 
 * if not !Flag_parsing_php.type_hints_extension
 * then raise Parsing.Parse_error;
 * TypedDeclaration ($1, $2, Some ($3, $4), $5)
 * }
 *)
*/

 | TOBRACE inner_statement_list TCBRACE   { Block($1,$2,$3) }

 | T_IF TOPAR expr TCPAR statement elseif_list else_single 
     { If($1,($2,$3,$4),$5,$6,$7) }
 | T_IF TOPAR expr TCPAR TCOLON 
     inner_statement_list new_elseif_list new_else_single 
     T_ENDIF TSEMICOLON 
     { IfColon($1,($2,$3,$4),$5,$6,$7,$8,$9,$10)  }

 | T_WHILE TOPAR expr  TCPAR while_statement 
     { While($1,($2,$3,$4),$5) }
 | T_DO statement T_WHILE TOPAR expr TCPAR TSEMICOLON 
     { Do($1,$2,$3,($4,$5,$6),$7) }
 | T_FOR TOPAR for_expr TSEMICOLON  for_expr TSEMICOLON for_expr TCPAR
     for_statement 
     { For($1,$2,$3,$4,$5,$6,$7,$8,$9) }

 | T_SWITCH TOPAR expr TCPAR	switch_case_list 
     { Switch($1,($2,$3,$4),$5) }

 | T_FOREACH TOPAR variable T_AS
     foreach_variable foreach_optional_arg TCPAR 
     foreach_statement 
     { Foreach($1,$2,mk_e (Lv $3),$4,Left $5,$6,$7,$8) }
 | T_FOREACH TOPAR expr_without_variable T_AS
     variable foreach_optional_arg TCPAR 
     foreach_statement 
     { Foreach($1,$2,$3,$4,Right $5,$6,$7,$8)  }

 | T_BREAK TSEMICOLON       	{ Break($1,None,$2) }
 | T_BREAK expr TSEMICOLON	{ Break($1,Some $2, $3) }
 | T_CONTINUE TSEMICOLON	{ Continue($1,None,$2) }
 | T_CONTINUE expr TSEMICOLON	{ Continue($1,Some $2, $3) }

 | T_RETURN TSEMICOLON		              { Return ($1,None, $2) }
 | T_RETURN expr_without_variable TSEMICOLON  { Return ($1,Some ($2), $3)}
 | T_RETURN variable TSEMICOLON      { Return ($1,Some (mk_e (Lv $2)), $3)}

 | T_TRY   TOBRACE inner_statement_list TCBRACE
   T_CATCH TOPAR fully_qualified_class_name  T_VARIABLE TCPAR 
     TOBRACE inner_statement_list TCBRACE 
     additional_catches 
     { let try_block = ($2,$3,$4) in
       let catch_block = ($10, $11, $12) in
       let catch = ($5, ($6, ($7, DName $8), $9), catch_block) in
       Try($1, try_block, catch, $13)
     }
 | T_THROW expr TSEMICOLON { Throw($1,$2,$3) }

 | T_ECHO echo_expr_list TSEMICOLON     { Echo($1,$2,$3) }
 | T_INLINE_HTML			{ InlineHtml($1) }

 | T_OPEN_TAG_WITH_ECHO expr T_CLOSE_TAG_OF_ECHO { 
     (* todo? cheat ..., ugly, the 2 tokens will have a wrong string *)
     Echo ($1, [Left $2], $3)
   }

 | T_GLOBAL global_var_list TSEMICOLON { Globals($1,$2,$3) }
 | T_STATIC static_var_list TSEMICOLON { StaticVars($1,$2,$3) }

 | T_UNSET TOPAR unset_variables TCPAR TSEMICOLON { Unset($1,($2,$3,$4),$5) }

 | T_USE use_filename TSEMICOLON		  { Use($1,$2,$3) }
 | T_DECLARE  TOPAR declare_list TCPAR declare_statement 
     { Declare($1,($2,$3,$4),$5) }

/*(*x: GRAMMAR statement *)*/
/*(*----------------------------*)*/
/*(*2 auxillary statements *)*/
/*(*----------------------------*)*/
for_expr:
 | /*(*empty*)*/    	{ [] }
 | non_empty_for_expr	{ $1 }

foreach_optional_arg:
  | /*(*empty*)*/			{ None }
  | T_DOUBLE_ARROW foreach_variable	{ Some($1,$2) }

foreach_variable: is_reference variable { ($1, $2) }

switch_case_list:
 | TOBRACE            case_list TCBRACE  { CaseList($1,None,$2,$3) }
 | TOBRACE TSEMICOLON case_list TCBRACE  { CaseList($1, Some $2, $3, $4) }
 | TCOLON             case_list T_ENDSWITCH TSEMICOLON	
     { CaseColonList($1,None,$2, $3, $4) }
 | TCOLON TSEMICOLON  case_list T_ENDSWITCH TSEMICOLON	
     { CaseColonList($1, Some $2, $3, $4, $5) }

 | T_XHP_COLONID_DEF { failwith_xhp_ambiguity_colon (snd $1) }

case_list: case_list_rev { List.rev $1 }
case_list_rev:
 | /*(*empty*)*/	{ [] } 
 | case_list_rev    T_CASE expr case_separator inner_statement_list 
     { Case($2,$3,$4,$5)::$1   }
 | case_list_rev    T_DEFAULT   case_separator inner_statement_list 
     { Default($2,$3,$4)::$1 }

case_separator:
 | TCOLON     { $1 }
 /*(* ugly php ... *)*/
 | TSEMICOLON { $1 }

 | T_XHP_COLONID_DEF { failwith_xhp_ambiguity_colon (snd $1) }


while_statement:
 | statement                                         { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDWHILE TSEMICOLON { ColonStmt($1,$2,$3,$4) }

for_statement:
 | statement                                       { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDFOR TSEMICOLON { ColonStmt($1,$2,$3,$4) }

foreach_statement:
 | statement                                           { SingleStmt $1 } 
 | TCOLON inner_statement_list T_ENDFOREACH TSEMICOLON { ColonStmt($1,$2,$3,$4)}

declare_statement:
 | statement                                           { SingleStmt $1 }
 | TCOLON inner_statement_list T_ENDDECLARE TSEMICOLON { ColonStmt($1,$2,$3,$4)}


elseif_list:
 | /*(*empty*)*/ { [] }
 | elseif_list  T_ELSEIF TOPAR expr TCPAR statement { $1 ++ [$2,($3,$4,$5),$6] }

new_elseif_list:
 | /*(*empty*)*/ { [] }
 | new_elseif_list    T_ELSEIF TOPAR expr TCPAR TCOLON inner_statement_list 
     { $1 ++ [$2,($3,$4,$5),$6,$7] }


else_single:
 | /*(*empty*)*/    { None }
 | T_ELSE statement { Some($1,$2) }

new_else_single:
 | /*(*empty*)*/                      { None }
 | T_ELSE TCOLON inner_statement_list { Some($1,$2,$3) }


additional_catch:
 | T_CATCH TOPAR fully_qualified_class_name T_VARIABLE TCPAR 
           TOBRACE inner_statement_list TCBRACE 
     { let catch_block = ($6, $7, $8) in
       let catch = ($1, ($2, ($3, DName $4), $5), catch_block) in
       catch
     }
/*(*x: GRAMMAR statement *)*/
/*(*----------------------------*)*/
/*(*2 auxillary bis *)*/
/*(*----------------------------*)*/

declare: ident   TEQ static_scalar { Name $1, ($2, $3) }

global_var:
 | T_VARIABLE			{ GlobalVar (DName $1) }
 | TDOLLAR r_variable		{ GlobalDollar ($1, $2) }
 | TDOLLAR TOBRACE expr TCBRACE	{ GlobalDollarExpr ($1, ($2, $3, $4)) }

/*(* can not factorize, otherwise shift/reduce conflict *)*/
static_var_list: static_var_list_rev { List.rev $1 }
static_var_list_rev:
 | T_VARIABLE                   { [Left (DName $1, None)] }
 | T_VARIABLE TEQ static_scalar { [Left (DName $1, Some ($2, $3)) ] }
 | static_var_list_rev TCOMMA   T_VARIABLE 
     { Left (DName $3, None)::Right $2::$1 }
 | static_var_list_rev TCOMMA   T_VARIABLE TEQ static_scalar 
     { Left (DName $3, Some ($4, $5))::Right $2::$1 }

unset_variable: variable	{ $1 }

use_filename:
 |       T_CONSTANT_ENCAPSED_STRING		{ UseDirect $1 }
 | TOPAR T_CONSTANT_ENCAPSED_STRING TCPAR	{ UseParen ($1, $2, $3) }
/*(*e: GRAMMAR statement *)*/

/*(*************************************************************************)*/
/*(*1 Constant declaration *)*/
/*(*************************************************************************)*/

 /*(* PHP 5.3 *)*/
constant_declaration_statement:
 | T_CONST T_IDENT TEQ static_scalar TSEMICOLON 
     { ($1, Name $2, $3, $4, $5) }

/*(*************************************************************************)*/
/*(*1 Function declaration *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR function declaration *)*/
function_declaration_statement:	unticked_function_declaration_statement	{ $1 }

unticked_function_declaration_statement:
  T_FUNCTION is_reference ident
  TOPAR parameter_list TCPAR 
  /*(* static-php-ext: *)*/
  optional_class_type
  TOBRACE inner_statement_list TCBRACE
  { 
    let params = ($4, $5, $6) in
    let body = ($8, $9, $10) in

    if not !Flag_parsing_php.type_hints_extension && $7 <> None
    then raise Parsing.Parse_error;
    ({ f_tok = $1; f_ref = $2; f_name = Name $3; f_params = params;
       f_return_type = $7;f_body = body;
    })
  }
/*(*x: GRAMMAR function declaration *)*/
/*(* can not factorize, otherwise shift/reduce conflict *)*/
non_empty_parameter_list:
 | optional_class_type T_VARIABLE			
     { let p = mk_param $1 $2 in [Left3 p] }
 | optional_class_type TAND T_VARIABLE			
     { let p = mk_param $1 $3 in [Left3 {p with p_ref = Some $2}] }
 | optional_class_type T_VARIABLE         TEQ static_scalar
     { let p = mk_param $1 $2 in [Left3 {p with p_default = Some ($3,$4)}] }
 | optional_class_type TAND T_VARIABLE    TEQ static_scalar
     { let p = mk_param $1 $3 in 
       [Left3 {p with p_ref = Some $2; p_default = Some ($4, $5)}]
     }
 /*(* sgrep_ext: *)*/
 | TDOTS                                 
     { sgrep_guard ([Middle3 $1]) }
 | non_empty_parameter_list TCOMMA TDOTS 
     { sgrep_guard ($1 ++ [Right3 $2; Middle3 $3]) }

 /*(*s: repetitive non_empty_parameter_list *)*/
  | non_empty_parameter_list TCOMMA  optional_class_type T_VARIABLE 	
      { let p = mk_param $3 $4 in $1 ++ [Right3 $2; Left3 p] }
  | non_empty_parameter_list TCOMMA  optional_class_type TAND T_VARIABLE	
      { let p = mk_param $3 $5 in $1 ++ [Right3 $2; Left3 {p with p_ref = Some $4}] }
  | non_empty_parameter_list TCOMMA  optional_class_type T_VARIABLE TEQ static_scalar 	
      { let p = mk_param $3 $4 in $1 ++ [Right3 $2; Left3 {p with p_default = Some ($5,$6)}] }
  | non_empty_parameter_list TCOMMA  optional_class_type TAND T_VARIABLE	 TEQ static_scalar 
      { let p = mk_param $3 $5 in 
        $1 ++ [Right3 $2; Left3 {p with p_ref = Some $4; p_default = Some ($6, $7)}]
      }

 /*(*e: repetitive non_empty_parameter_list *)*/
/*(*x: GRAMMAR function declaration *)*/
optional_class_type:
 | /*(*empty*)*/	{ None }
 | type_hint            { Some $1 }

type_hint:
 | class_name_or_selfparent { Hint $1 }
 | T_ARRAY		    { HintArray $1 }

is_reference:
 | /*(*empty*)*/  { None }
 | TAND		  { Some $1 }

/*(* PHP 5.3 *)*/
lexical_vars:
 | /*(*empty*)*/  { None }
 | T_USE TOPAR lexical_var_list TCPAR { 
     Some ($1, ($2, ($3 +> List.map (function
     | Right info -> Right info
     | Left (a,b) -> Left (LexicalVar (a,b)))), $4)) }

lexical_var_list:
 | T_VARIABLE				{ [Left (None, DName $1)] }
 | TAND T_VARIABLE			{ [Left (Some $1, DName $2)] }
 | lexical_var_list TCOMMA T_VARIABLE       { $1 ++ [Right $2; Left (None, DName $3)]  }
 | lexical_var_list TCOMMA TAND T_VARIABLE  { $1 ++ [Right $2; Left (Some $3, DName $4)] }
/*(*e: GRAMMAR function declaration *)*/

/*(*************************************************************************)*/
/*(*1 Class declaration *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR class declaration *)*/
class_declaration_statement: unticked_class_declaration_statement { $1 }

unticked_class_declaration_statement:
 | class_entry_type class_name 
     extends_from implements_list
     TOBRACE  class_statement_list TCBRACE 
     { { c_type = $1; c_name = $2;c_extends = $3; 
         c_implements = $4; c_body = $5, $6, $7;
       } }

 | interface_entry class_name
     interface_extends_list
     TOBRACE class_statement_list TCBRACE 
     { { c_type = Interface $1; c_name = $2; c_extends = None; 
         (* we use c_implements for interface extension because
          * it can be a list. ugly?
          *)
         c_implements = $3; c_body = $4, $5, $6; } }

trait_declaration_statement:
 | T_TRAIT ident TOBRACE class_statement_list TCBRACE 
     { { c_type = Trait $1; c_name = (Name $2); c_extends = None;
         c_implements = None; c_body = ($3, $4, $5) } }

/*(*x: GRAMMAR class declaration *)*/
class_name: 
 | ident { Name $1 }
 /*(*s: class_name grammar rule hook *)*/
  /*(* xhp: an XHP element def *)*/
  | T_XHP_COLONID_DEF { XhpName $1 }
 /*(*e: class_name grammar rule hook *)*/

class_entry_type:
 | T_CLASS  	      { ClassRegular $1 }
 | T_ABSTRACT T_CLASS { ClassAbstract ($1, $2) }
 | T_FINAL    T_CLASS { ClassFinal ($1, $2) }

interface_entry: 
 | T_INTERFACE		{ $1 }
/*(*x: GRAMMAR class declaration *)*/
extends_from:
 | /*(*empty*)*/			{ None }
 | T_EXTENDS fully_qualified_class_name	{ Some ($1, $2) }

interface_extends_list:
 | /*(*empty*)*/            { None }
 | T_EXTENDS interface_list { Some($1,$2) }

implements_list:
 | /*(*empty*)*/               { None }
 | T_IMPLEMENTS interface_list { Some($1, $2) }
/*(*x: GRAMMAR class declaration *)*/
/*(*----------------------------*)*/
/*(*2 class statement *)*/
/*(*----------------------------*)*/

class_statement:
 | T_CONST class_constant_declaration            TSEMICOLON 
     { ClassConstants($1, $2, $3) }
 | variable_modifiers class_variable_declaration TSEMICOLON 
     { ClassVariables($1, None, $2, $3) }

  /*(* static-php-ext: *)*/
 | variable_modifiers type_hint class_variable_declaration TSEMICOLON 
     { 
       if not !Flag_parsing_php.facebook_lang_extensions
       then raise Parsing.Parse_error;
       ClassVariables($1, Some $2, $3, $4) 
     }

 | method_modifiers T_FUNCTION is_reference ident_method  
     TOPAR parameter_list TCPAR
     optional_class_type
     method_body 
     { 
       if not !Flag_parsing_php.type_hints_extension && $8 <> None
       then raise Parsing.Parse_error;
       Method {
         m_modifiers = $1; m_tok = $2; m_ref = $3; m_name = Name $4;
         m_params = ($5, $6, $7); m_return_type = $8; m_body = $9;
       }
     }

 | T_XHP_ATTRIBUTE xhp_attribute_decls TSEMICOLON 
     { XhpDecl (XhpAttributesDecl ($1, $2, $3)) }
 | T_XHP_CHILDREN  xhp_children_decl  TSEMICOLON 
     { XhpDecl (XhpChildrenDecl ($1, $2, $3)) }
 | T_XHP_CATEGORY xhp_category_list TSEMICOLON 
     { XhpDecl (XhpCategoriesDecl ($1, $2, $3)) }
/*(* php 5.4 traits *)*/
 | T_USE trait_list TSEMICOLON 
     { UseTrait ($1, $2, Left $3) }
 | T_USE trait_list TOBRACE trait_rules TCBRACE 
     { UseTrait ($1, $2, Right ($3, $4, $5)) }

/*(* ugly, php allows method names which should be IMHO reserved keywords *)*/
ident_method: 
 | ident { $1 }
 | T_PARENT { "parent", $1 }
 | T_SELF   { "self", $1 }

/*(*x: GRAMMAR class declaration *)*/
class_constant_declaration:
 | ident TEQ static_scalar	
     { [Left ((Name $1), ($2, $3))] }
 | class_constant_declaration TCOMMA      ident TEQ static_scalar	
     { $1 ++ [Right $2; Left ((Name $3, ($4, $5)))] }


variable_modifiers:
 | T_VAR				{ NoModifiers $1 }
 | non_empty_member_modifiers		{ VModifiers $1 }


/*(* can not factorize, otherwise shift/reduce conflict *)*/
class_variable_declaration:
 | T_VARIABLE			{ [Left (DName $1, None)] }
 | T_VARIABLE TEQ static_scalar	{ [Left (DName $1, Some ($2, $3))] }

 /*(*s: repetitive class_variable_declaration with comma *)*/
  | class_variable_declaration TCOMMA T_VARIABLE				
      { $1 ++ [Right $2; Left (DName $3, None)] }
  | class_variable_declaration TCOMMA T_VARIABLE TEQ static_scalar	
      { $1 ++ [Right $2; Left (DName $3, Some ($4, $5))] }
 /*(*e: repetitive class_variable_declaration with comma *)*/
/*(*x: GRAMMAR class declaration *)*/
member_modifier:
 | T_PUBLIC    { Public,($1) } | T_PROTECTED { Protected,($1) }
 | T_PRIVATE   { Private,($1) }
 | T_STATIC    { Static,($1) }
 | T_ABSTRACT { Abstract,($1) } | T_FINAL{ Final,($1) }

method_body:
 | TSEMICOLON                   	{ AbstractMethod $1 }
 | TOBRACE inner_statement_list TCBRACE	{ MethodBody ($1, $2, $3) }

/*(*----------------------------*)*/
/*(*2 XHP attributes *)*/
/*(*----------------------------*)*/
/*(* mostly a copy paste of the original XHP grammar *)*/

xhp_attribute_decl:
 | T_XHP_COLONID_DEF 
     { XhpAttrInherit $1 }
 | xhp_attribute_decl_type xhp_attr_name xhp_attribute_default
     xhp_attribute_is_required 
     { XhpAttrDecl ($1, ((Ast.str_of_info $2, $2)), $3, $4) }

/*(* In the original grammar each types, e.g. float/string/bool/... 
   * had their special token. I abuse T_IDENT here, except for 
   * enum which needs a special grammar rule.
   *)*/
xhp_attribute_decl_type:
 | class_name   { XhpAttrType $1 }
 | T_VAR        { XhpAttrType (Name (Ast.str_of_info $1, $1)) }
 | T_ARRAY      { XhpAttrType (Name (Ast.str_of_info $1, $1)) }
 | T_XHP_ENUM TOBRACE xhp_enum_list TCBRACE 
     { XhpAttrEnum ($1, ($2, $3, $4)) }

xhp_attribute_default:
 | /*(*empty*)*/     { None }
 | TEQ static_scalar { Some ($1, $2) }

xhp_attribute_is_required:
 | /*(*empty*)*/  { None }
 | T_XHP_REQUIRED { Some $1 }

xhp_enum:
 | common_scalar { $1 }

/*(* was called xhp_label_pass in original grammar *)*/
xhp_attr_name:
 | xhp_attr_name_atom { $1 }
 /*(* ugly, but harder to lex foo-name as a single token without
    * introducing lots of ambiguities. It's ok for :foo:bar but not
    * for attribute name.
    * 
    * todo? could check that there is no whitespace between those
    * tokens.
    *)*/
 | xhp_attr_name TMINUS xhp_attr_name_atom 
     { let s = Ast.str_of_info $1 ^  Ast.str_of_info $2 ^ Ast.str_of_info $3 in
       Ast.rewrap_str s $1
     }

xhp_attr_name_atom:
 /*(* could put T_IDENT but even XHP keywords are accepted as XHP attributes*)*/
 | ident { snd $1 }

 /*(* Just like it's ok (but not good IMHO) to use XHP keywords in place
    * of regular PHP idents, it's ok to use PHP keywords in place
    * of XHP attribute names (but again not good IMHO).
    * 
    * The list of tokens below are all identifier-like keywords mentioned in
    * the 'keyword tokens' section at the beginning of this file
    * (which roughly correspond to the tokens in Lexer_php.keywords_table).
    * There is no conflict introducing this big list of tokens.
    * 
    * todo? emit a warning when the user use PHP keywords for XHP attribute ?
    *)*/
 | T_ECHO { $1 } | T_PRINT { $1 } | T_IF { $1 } | T_ELSE { $1 }
 | T_ELSEIF { $1 } | T_ENDIF { $1 } | T_DO { $1 } | T_WHILE { $1 }
 | T_ENDWHILE { $1 } | T_FOR { $1 } | T_ENDFOR { $1 } | T_FOREACH { $1 }
 | T_ENDFOREACH { $1 } | T_SWITCH { $1 } | T_ENDSWITCH { $1 } | T_CASE { $1 }
 | T_DEFAULT { $1 } | T_BREAK { $1 } | T_CONTINUE { $1 } | T_RETURN { $1 }
 | T_TRY { $1 } | T_CATCH { $1 } | T_THROW { $1 } | T_EXIT { $1 }
 | T_DECLARE { $1 } | T_ENDDECLARE { $1 } | T_USE { $1 } | T_GLOBAL { $1 }
 | T_AS { $1 } | T_FUNCTION { $1 } | T_CONST { $1 } | T_STATIC { $1 }
 | T_ABSTRACT { $1 } | T_FINAL { $1 } | T_PRIVATE { $1 } | T_PROTECTED { $1 }
 | T_PUBLIC { $1 } | T_VAR { $1 } | T_UNSET { $1 } | T_ISSET { $1 }
 | T_EMPTY { $1 } | T_CLASS { $1 }
 | T_INTERFACE { $1 } | T_EXTENDS { $1 } | T_IMPLEMENTS { $1 } | T_LIST { $1 }
 | T_ARRAY { $1 } | T_CLASS_C { $1 } | T_METHOD_C { $1 } | T_FUNC_C { $1 }
 | T_LINE { $1 } | T_FILE { $1 } | T_LOGICAL_OR { $1 } | T_LOGICAL_AND { $1 }
 | T_LOGICAL_XOR { $1 } | T_NEW { $1 } | T_CLONE { $1 } | T_INSTANCEOF { $1 }
 | T_INCLUDE { $1 } | T_INCLUDE_ONCE { $1 } | T_REQUIRE { $1 }
 | T_REQUIRE_ONCE { $1 } | T_EVAL { $1 } | T_SELF { $1 } | T_PARENT { $1 }    
 | T_TRAIT { $1 } | T_INSTEADOF { $1 } | T_TRAIT_C { $1 }

/*(*----------------------------*)*/
/*(*2 XHP children *)*/
/*(*----------------------------*)*/
/*(* Mostly a copy paste of the original XHP grammar.
   * Not sure why it needs to be that complicated. We could factorize
   * rules for instance for the parenthesis stuff.
   *)*/
xhp_children_decl:
 | T_XHP_ANY { XhpChildAny $1 }
 | T_EMPTY   { XhpChildEmpty $1 }
 | xhp_children_paren_expr { $1 }

xhp_children_paren_expr:
 | TOPAR xhp_children_decl_expr TCPAR 
     { XhpChildParen ($1, $2, $3) }  
 | TOPAR xhp_children_decl_expr TCPAR TMUL 
     { XhpChildMul (XhpChildParen ($1, $2, $3), $4) }  
 | TOPAR xhp_children_decl_expr TCPAR TQUESTION 
     { XhpChildOption (XhpChildParen ($1, $2, $3), $4) }  
 | TOPAR xhp_children_decl_expr TCPAR TPLUS 
     { XhpChildPlus (XhpChildParen ($1, $2, $3), $4) }  

xhp_children_decl_expr:
 | xhp_children_paren_expr { $1 }
 | xhp_children_decl_tag { $1 }
 | xhp_children_decl_tag TMUL      { XhpChildMul ($1, $2)  }
 | xhp_children_decl_tag TQUESTION { XhpChildOption ($1, $2) }
 | xhp_children_decl_tag TPLUS     { XhpChildPlus ($1, $2) }

 | xhp_children_decl_expr TCOMMA xhp_children_decl_expr 
     { XhpChildSequence ($1, $2, $3) }
 | xhp_children_decl_expr TOR xhp_children_decl_expr 
     { XhpChildAlternative ($1, $2, $3) }

xhp_children_decl_tag:
 | T_XHP_ANY           { XhpChildAny ($1) }
 | T_XHP_PCDATA        { XhpChildPcdata ($1) }
 | T_XHP_COLONID_DEF   { XhpChild $1 }
 | T_XHP_PERCENTID_DEF { XhpChildCategory $1 }

/*(*----------------------------*)*/
/*(*2 XHP category *)*/
/*(*----------------------------*)*/

xhp_category:
 | T_XHP_PERCENTID_DEF { $1 }

/*(*----------------------------*)*/
/*(*2 Traits *)*/
/*(*----------------------------*)*/

trait_rule:
 | trait_precedence_rule  { $1 }
 | trait_alias_rule       { $1 }

trait_precedence_rule:
 | class_namespace_string TCOLCOL T_IDENT T_INSTEADOF trait_list TSEMICOLON
   { raise Todo }

trait_alias_rule:
 | trait_alias_rule_method T_AS method_modifiers T_IDENT TSEMICOLON
   { raise Todo }

 | trait_alias_rule_method T_AS non_empty_member_modifiers TSEMICOLON
   { raise Todo }

trait_alias_rule_method:
 | class_namespace_string TCOLCOL T_IDENT { raise Todo }
 | T_IDENT { raise Todo }

/*(*e: GRAMMAR class declaration *)*/
/*(*************************************************************************)*/
/*(*1 Expressions (and variables) *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR expression *)*/
/*(* a little coupling with non_empty_function_call_parameter_list *)*/
expr:
 | r_variable				{ mk_e (Lv $1) }
 | expr_without_variable		{ $1 }

expr_without_variable: 
 | expr_without_variable_bis { mk_e $1 }

 /*(* xhp: This extension fix a deficiency in the original PHP grammar
    * which does not allow to do things like foo()[2]. Array accesses
    * in PHP were allowed only for variables.
    *
    * The rule below generates 3 s/r conflicts. 
    *
    * I thought I could remove some conflicts by putting the rule
    * closer to the funcall_call rule, which does not
    * generate any conflict, but it then can not handle
    * code like $o->foo()['foo'].
    * 
    * I then thought I could put it closer to variable2, 
    * but it must be 'variable' not 'variable2' otherwise
    * it can not process code like foo()['fld1']['fld2'].
    * 
    * In fact it can not be put close to 'variable' either because
    * then it can not parse code like ($this->getAssocData($user))[0];
    * So the right place is here, in expr_without_variable.
    *)*/
  | expr TOBRA dim_offset TCBRA
   { 
     match $1 with
     (* Lv corresponds to Lvalue which includes function calls so 
      * foo()[1] will be translated into a 
      * VArrayAccess(FunCallSimple(...), 1).
      *)
     | Lv v ->
         let var = (VArrayAccess (v, ($2, $3, $4))) in
         mk_e (Lv var)

     (* The 'lvalue' type was originally restricted to variables and 
      * function/method calls. It could not cope with
      * VArrayAccess of arbitrary expressions such as 
      * "array(1,2,3)" which are ConsArray of the 'expr' type.
      * 
      * So for the general case we could have transformed
      * such array access into calls to __xhp_idx(), just like
      * in the original XHP parser, and introduced some fake tokens
      * in the Ast. But having fake tokens break some
      * of the assumptions we have later when we build the database of code
      * so it's simpler to introduce a new constructor, 
      * VarrayAccessXhp that can accept expressions. This forces us
      * to add extra code to handle yet another constructor, but
      * the __xhp_idx() solution would also force us, to have precise
      * analysis, to special case such function calls anyway.
      * 
      * An alternative would be to rethink ast_php.ml and
      * merge the 'expr' and 'lvalue' types together.
      *)
     | _ ->
         let var = (VArrayAccessXhp ($1, ($2, $3, $4))) in
         mk_e (Lv var)
   }

/*(*x: GRAMMAR expression *)*/
expr_without_variable_bis:
 | scalar				{ Sc $1 }

 | TOPAR expr TCPAR 	{ ParenExpr($1,$2,$3) }

 | variable TEQ expr		{ Assign($1,$2,$3) }
 | variable TEQ TAND variable   { AssignRef($1,$2,$3,$4) }
 | variable TEQ TAND T_NEW class_name_reference ctor_arguments 
     { AssignNew($1,$2,$3,$4,$5,$6) }


 | variable T_PLUS_EQUAL   expr	{ AssignOp($1,(AssignOpArith Plus,$2),$3) }
 | variable T_MINUS_EQUAL  expr	{ AssignOp($1,(AssignOpArith Minus,$2),$3) }
 | variable T_MUL_EQUAL    expr	{ AssignOp($1,(AssignOpArith Mul,$2),$3) }
 | variable T_DIV_EQUAL    expr	{ AssignOp($1,(AssignOpArith Div,$2),$3) }
 | variable T_MOD_EQUAL    expr	{ AssignOp($1,(AssignOpArith Mod,$2),$3) }
 | variable T_AND_EQUAL    expr	{ AssignOp($1,(AssignOpArith And,$2),$3) }
 | variable T_OR_EQUAL     expr	{ AssignOp($1,(AssignOpArith Or,$2),$3) }
 | variable T_XOR_EQUAL    expr	{ AssignOp($1,(AssignOpArith Xor,$2),$3) }
 | variable T_SL_EQUAL     expr	{ AssignOp($1,(AssignOpArith DecLeft,$2),$3) }
 | variable T_SR_EQUAL     expr	{ AssignOp($1,(AssignOpArith DecRight,$2),$3) }

 | variable T_CONCAT_EQUAL expr	{ AssignOp($1,(AssignConcat,$2),$3) }

 | rw_variable T_INC { Postfix($1, (Inc, $2)) }
 | rw_variable T_DEC { Postfix($1, (Dec, $2)) }
 | T_INC rw_variable { Infix((Inc, $1),$2) }
 | T_DEC rw_variable { Infix((Dec, $1),$2) }

 | expr T_BOOLEAN_OR expr   { Binary($1,(Logical OrBool ,$2),$3) }
 | expr T_BOOLEAN_AND  expr { Binary($1,(Logical AndBool,$2),$3) }
 | expr T_LOGICAL_OR  expr  { Binary($1,(Logical OrLog,  $2),$3) }
 | expr T_LOGICAL_AND  expr { Binary($1,(Logical AndLog, $2),$3) }
 | expr T_LOGICAL_XOR expr  { Binary($1,(Logical XorLog, $2),$3) }

 | expr TPLUS expr 	{ Binary($1,(Arith Plus ,$2),$3) }
 | expr TMINUS expr 	{ Binary($1,(Arith Minus,$2),$3) }
 | expr TMUL expr	{ Binary($1,(Arith Mul,$2),$3) }
 | expr TDIV expr	{ Binary($1,(Arith Div,$2),$3) }
 | expr TMOD expr 	{ Binary($1,(Arith Mod,$2),$3) }

 | expr T_XHP_PERCENTID_DEF 	{ failwith_xhp_ambiguity_percent (snd $2) }

 | expr TAND expr	{ Binary($1,(Arith And,$2),$3) }
 | expr TOR expr	{ Binary($1,(Arith Or,$2),$3) }
 | expr TXOR expr	{ Binary($1,(Arith Xor,$2),$3) }
 | expr T_SL expr	{ Binary($1,(Arith DecLeft,$2),$3) }
 | expr T_SR expr	{ Binary($1,(Arith DecRight,$2),$3) }

 | expr TDOT expr 	{ Binary($1,(BinaryConcat,$2),$3) }

 | expr T_IS_IDENTICAL        expr { Binary($1,(Logical Identical,$2),$3) }
 | expr T_IS_NOT_IDENTICAL    expr { Binary($1,(Logical NotIdentical,$2),$3) }
 | expr T_IS_EQUAL            expr { Binary($1,(Logical Eq,$2),$3) }
 | expr T_IS_NOT_EQUAL        expr { Binary($1,(Logical NotEq,$2),$3) }
 | expr TSMALLER              expr { Binary($1,(Logical Inf,$2),$3) }
 | expr T_IS_SMALLER_OR_EQUAL expr { Binary($1,(Logical InfEq,$2),$3) }
 | expr TGREATER              expr { Binary($1,(Logical Sup,$2),$3) }
 | expr T_IS_GREATER_OR_EQUAL expr { Binary($1,(Logical SupEq,$2),$3) }

 | TPLUS  expr    %prec T_INC           { Unary((UnPlus,$1),$2) }
 | TMINUS expr    %prec T_INC           { Unary((UnMinus,$1),$2) }
 | TBANG  expr                          { Unary((UnBang,$1),$2) }
 | TTILDE expr                          { Unary((UnTilde,$1),$2) }


 | T_LIST TOPAR assignment_list TCPAR TEQ expr 
     { AssignList($1,($2,$3,$4),$5,$6) }
 | T_ARRAY TOPAR array_pair_list TCPAR 
     { ArrayLong($1,($2,$3,$4)) }
 | TOBRA array_pair_list TCBRA
     { ArrayShort($1, $2, $3) }

 | T_NEW class_name_reference ctor_arguments 
     { New($1,$2,$3) }
 | T_CLONE expr { Clone($1,$2) }
 | expr T_INSTANCEOF class_name_reference 
     { InstanceOf($1,$2,$3) }

 | expr TQUESTION  expr TCOLON  expr	 { CondExpr($1,$2,Some $3,$4,$5) }
 /*(* PHP 5.3 *)*/
 | expr TQUESTION  TCOLON  expr	 { CondExpr($1,$2,None,$3,$4) }

/*(* I don't parse XHP elements defs in the same way than the original
   * XHP parser, which simplifies the grammar, but introduce possible
   * ambiguities. See tests/xhp_pb_but_ok/colon_ambiguity*.php
   * I don't want to solve those ambiguities but I can at least print a
   * useful parsing error message with those fake rules.
   * 
   * Everywhere in this grammar where we use TCOLON we should add 
   * an error rule similar to the one below.
   *)*/
 | expr TQUESTION  expr T_XHP_COLONID_DEF 
     { failwith_xhp_ambiguity_colon (snd $4) }

 | T_BOOL_CAST   expr	{ Cast((BoolTy,$1),$2) }
 | T_INT_CAST    expr 	{ Cast((IntTy,$1),$2) }
 | T_DOUBLE_CAST expr 	{ Cast((DoubleTy,$1),$2) }
 | T_STRING_CAST expr	{ Cast((StringTy,$1),$2) }
 | T_ARRAY_CAST  expr 	{ Cast((ArrayTy,$1),$2) }
 | T_OBJECT_CAST expr 	{ Cast((ObjectTy,$1),$2) }


 | T_UNSET_CAST  expr	{ CastUnset($1,$2) }

 | T_EXIT exit_expr	{ Exit($1,$2) }
 | T__AT expr           { At($1,$2) }
 | T_PRINT expr  { Print($1,$2) }

 | TBACKQUOTE encaps_list TBACKQUOTE   { BackQuote($1,$2,$3) }
 /*(* PHP 5.3 *)*/
 | T_FUNCTION is_reference TOPAR parameter_list TCPAR lexical_vars 
   TOBRACE inner_statement_list TCBRACE 
     { let params = ($3, $4, $5) in
       let body = ($7, $8, $9) in
       let ldef = {
         l_tok = $1; l_ref = $2; l_params = params; l_use = $6; l_body = body;
       }
       in
       Lambda ldef
     }
 /*(* php-facebook-ext: todo? in hphp.y yield are at the statement level
    * and are restricted to a few forms *)*/
 | T_YIELD expr { Yield ($1, $2) }
 | T_YIELD T_BREAK { YieldBreak ($1, $2) }

 | internal_functions_in_yacc { $1 }

 /*(*s: exprbis grammar rule hook *)*/
 /*(* sgrep_ext: *)*/
 | TDOTS { sgrep_guard (SgrepExprDots $1) }

 /*(* xhp: do not put in 'expr', otherwise can't have xhp 
    * in function arguments
    *)*/
 | xhp_html { XhpHtml $1 }

 /*(*e: exprbis grammar rule hook *)*/
/*(*x: GRAMMAR expression *)*/
/*(*pad: why this name ? *)*/
internal_functions_in_yacc:
 | T_INCLUDE      expr 		       { Include($1,$2) }
 | T_INCLUDE_ONCE expr 	               { IncludeOnce($1,$2) }
 | T_REQUIRE      expr		       { Require($1,$2) }
 | T_REQUIRE_ONCE expr		       { RequireOnce($1,$2) }

 | T_ISSET TOPAR isset_variables TCPAR { Isset($1,($2,$3,$4)) }
 | T_EMPTY TOPAR variable TCPAR	       { Empty($1,($2,$3,$4)) }

 | T_EVAL TOPAR expr TCPAR 	       { Eval($1,($2,$3,$4)) }
/*(*x: GRAMMAR expression *)*/
/*(*----------------------------*)*/
/*(*2 scalar *)*/
/*(*----------------------------*)*/

/*(*s: GRAMMAR scalar *)*/
scalar:
 | common_scalar		{ C $1 }
 | ident 			{ C (CName (Name $1)) }
 | class_constant	        { ClassConstant (fst $1, snd $1) }

 | TGUIL encaps_list TGUIL 	
     { Guil ($1, $2, $3)}
 | T_START_HEREDOC encaps_list T_END_HEREDOC 
     { HereDoc ($1, $2, $3) }

 /*(* generated by lexer for special case of ${beer}s. So it's really
    * more a variable than a constant. So I've decided to inline this
    * special case rule in encaps. Maybe this is too restrictive.
    *)*/
  /*(* | T_STRING_VARNAME {  raise Todo } *)*/

/*(*x: GRAMMAR scalar *)*/
static_scalar: /* compile-time evaluated scalars */
 | common_scalar	 { Sc (C $1) }
 | ident 		 { Sc (C (CName (Name $1))) }
 | static_class_constant { Sc (ClassConstant (fst $1, snd $1)) }

 | TPLUS static_scalar	 { Unary ((UnPlus, $1),$2)  }
 | TMINUS static_scalar	 { Unary ((UnMinus, $1),$2) }
 /*(* arrays are considered scalars in the PHP grammar, brilliant *)*/
 | T_ARRAY TOPAR static_array_pair_list TCPAR
     { ArrayLong($1, ($2, $3, $4)) }
 | TOBRA static_array_pair_list TCBRA
     { ArrayShort($1, $2, $3) }
 /*(* todo? ensure encaps_list contains only constant strings? *)*/
 | T_START_HEREDOC encaps_list T_END_HEREDOC 
     { Sc (HereDoc ($1, $2, $3)) }
 /*(*s: static_scalar grammar rule hook *)*/
  /* xdebug TODO AST  */
  | TDOTS { sgrep_guard (SgrepExprDots $1)  }
 /*(*e: static_scalar grammar rule hook *)*/



common_scalar:
 | T_LNUMBER 			{ Int($1) }
 | T_DNUMBER 			{ Double($1) }

 | T_CONSTANT_ENCAPSED_STRING	{ String($1) }

 | T_LINE { PreProcess(Line, $1) } 
 | T_FILE { PreProcess(File, $1) } | T_DIR { PreProcess(Dir, $1) }
 | T_CLASS_C { PreProcess(ClassC, $1) } | T_TRAIT_C { PreProcess(TraitC, $1)}
 | T_FUNC_C { PreProcess(FunctionC, $1) }|T_METHOD_C { PreProcess(MethodC, $1)}


 /*(*s: common_scalar grammar rule hook *)*/
  | T_CLASS_XDEBUG class_name TOBRACE class_statement_list TCBRACE { 
      XdebugClass ($2, $4)
    }
  | T_CLASS_XDEBUG class_name TOBRACE TDOTS TCBRACE { 
      XdebugClass ($2, [])
    }
  | T_CLASS_XDEBUG class_name TOBRACE TDOTS TSEMICOLON TCBRACE { 
      XdebugClass ($2, [])
    }
  | T_RESOURCE_XDEBUG  { XdebugResource }
 /*(*e: common_scalar grammar rule hook *)*/

class_constant: 
  | qualifier ident { $1, (Name $2) }

static_class_constant: class_constant { $1 } 
/*(*x: GRAMMAR scalar *)*/
/*(* can not factorize, otherwise shift/reduce conflict *)*/
static_array_pair_list: static_array_pair_list_rev { List.rev $1 }

non_empty_static_array_pair_list_rev:
 | static_scalar                              
     { [Left (ArrayExpr $1)] }
 | static_scalar T_DOUBLE_ARROW static_scalar
     { [Left (ArrayArrowExpr ($1,$2,$3))]}

 /*(*s: repetitive non_empty_static_array_pair_list *)*/
  | non_empty_static_array_pair_list_rev TCOMMA
      static_scalar 
      { Left (ArrayExpr $3)::Right $2::$1 }
  | non_empty_static_array_pair_list_rev TCOMMA
      static_scalar T_DOUBLE_ARROW static_scalar	
      { Left (ArrayArrowExpr ($3,$4,$5))::Right $2::$1 }
 /*(*e: repetitive non_empty_static_array_pair_list *)*/
/*(*e: GRAMMAR scalar *)*/

/*(*----------------------------*)*/
/*(*2 variable *)*/
/*(*----------------------------*)*/

/*(*s: GRAMMAR variable *)*/
variable: variable2 { variable2_to_lvalue $1 }
/*(*x: GRAMMAR variable *)*/
variable2:
 | base_variable_with_function_calls 
     { Variable ($1,[]) }
 | base_variable_with_function_calls 
     T_OBJECT_OPERATOR object_property method_or_not 
     variable_properties
     { Variable ($1, ($2, $3, $4)::$5) }
 /*(* sgrep_ext: *)*/
 | T_IDENT T_OBJECT_OPERATOR object_property method_or_not 
     variable_properties /*(* contains more T_OBJECT_OPERATOR *)*/
     { sgrep_guard (raise Todo) }

base_variable_with_function_calls:
 | base_variable {  BaseVar $1 }
 | function_call {  $1 }

base_variable:
 |             variable_without_objects                       
     { None,    $1 }
 | qualifier  variable_without_objects /*(*static_member*)*/ 
     { Some (Left3 $1), $2 }
 | variable_class_name TCOLCOL variable_without_objects 
     { Some (Right3 ($1, $2)), $3 }


variable_without_objects:
 |                             reference_variable { [], $1 }
 | simple_indirect_reference   reference_variable { $1, $2 }

reference_variable:
 | compound_variable			      { $1 }
 | reference_variable TOBRA dim_offset TCBRA  { VArrayAccess2($1, ($2,$3,$4)) }
 | reference_variable TOBRACE expr TCBRACE    { VBraceAccess2($1, ($2,$3,$4)) }

compound_variable:
 | T_VARIABLE			{ Var2 (DName $1, Ast.noScope()) }
 | TDOLLAR TOBRACE expr TCBRACE	{ VDollar2 ($1, ($2, $3, $4)) }

/*(*x: GRAMMAR variable *)*/
simple_indirect_reference:
 | TDOLLAR                           { [Dollar $1] }
 | simple_indirect_reference TDOLLAR { $1 ++ [Dollar $2] }

dim_offset:
 | /*(*empty*)*/   { None }
 | expr		   { Some $1 }
/*(*x: GRAMMAR variable *)*/
r_variable: variable { $1 }
w_variable: variable { $1 }
rw_variable: variable { $1 }
/*(*x: GRAMMAR variable *)*/
/*(*----------------------------*)*/
/*(*2 function call *)*/
/*(*----------------------------*)*/
function_call: function_head TOPAR function_call_parameter_list TCPAR
  { FunCall ($1, ($2, $3, $4)) }

 /*(*s: function_call grammar rule hook *)*/
 /*(*e: function_call grammar rule hook *)*/

/*(* cant factorize the rule with a qualifier_opt because it leads to
   * many conflicts :( *)*/
function_head:
 | ident                         { FuncName (None, Name $1) }
 | variable_without_objects      { FuncVar  (None, $1) }
 | qualifier    ident            { FuncName(Some $1, Name $2) }
 | qualifier    variable_without_objects  { FuncVar(Some $1, $2) }
/*(* PHP 5.3 *)*/
 | variable_class_name TCOLCOL ident { StaticMethodVar($1, $2, Name $3) }
 | variable_class_name TCOLCOL variable_without_objects { StaticObjVar ($1, $2, $3) }

/*(*x: GRAMMAR variable *)*/
/*(* can not factorize, otherwise shift/reduce conflict *)*/
non_empty_function_call_parameter_list:
 | variable			{ [Left (Arg (mk_e (Lv $1)))] }
 | expr_without_variable	{ [Left (Arg ($1))] }
 | TAND w_variable 		{ [Left (ArgRef($1,$2))] }

 /*(*s: repetitive non_empty_function_call_parameter_list *)*/
  | non_empty_function_call_parameter_list TCOMMA    variable
      { $1 ++ [Right $2; Left (Arg (mk_e (Lv $3)))] }
  | non_empty_function_call_parameter_list TCOMMA    expr_without_variable	
      { $1 ++ [Right $2; Left (Arg ($3))] }
  | non_empty_function_call_parameter_list TCOMMA    TAND w_variable
      { $1 ++ [Right $2; Left (ArgRef($3,$4))] }
 /*(*e: repetitive non_empty_function_call_parameter_list *)*/

/*(*x: GRAMMAR variable *)*/
/*(*----------------------------*)*/
/*(*2 list/array *)*/
/*(*----------------------------*)*/

assignment_list_element:
 | variable				{ ListVar $1 }
 | T_LIST TOPAR assignment_list TCPAR	{ ListList ($1, ($2, $3, $4)) }
 | /*(*empty*)*/			{ ListEmpty }
/*(*x: GRAMMAR variable *)*/
/*(* can not factorize, otherwise shift/reduce conflict *)*/
array_pair_list: array_pair_list_rev { List.rev $1 }
non_empty_array_pair_list_rev:
 | expr 				{ [Left (ArrayExpr $1)] }
 | TAND w_variable 			{ [Left (ArrayRef ($1,$2))] }
 | expr T_DOUBLE_ARROW   expr	        { [Left (ArrayArrowExpr($1,$2,$3))] }
 | expr T_DOUBLE_ARROW   TAND w_variable  { [Left (ArrayArrowRef($1,$2,$3,$4))] }

 /*(*s: repetitive non_empty_array_pair_list *)*/
  | non_empty_array_pair_list_rev TCOMMA   expr			
      { Left (ArrayExpr $3)::Right $2::$1 }
  | non_empty_array_pair_list_rev TCOMMA   TAND w_variable 
      { Left (ArrayRef ($3,$4))::Right $2::$1 }
  | non_empty_array_pair_list_rev TCOMMA   expr T_DOUBLE_ARROW expr	
      { Left (ArrayArrowExpr($3,$4,$5))::Right $2::$1 }
  | non_empty_array_pair_list_rev TCOMMA   expr T_DOUBLE_ARROW TAND w_variable 
      { Left (ArrayArrowRef($3,$4,$5,$6))::Right $2::$1 }
 /*(*e: repetitive non_empty_array_pair_list *)*/
/*(*x: GRAMMAR variable *)*/

/*(*----------------------------*)*/
/*(*2 XHP embeded html *)*/
/*(*----------------------------*)*/
xhp_html:
 | T_XHP_OPEN_TAG xhp_attributes T_XHP_GT xhp_children T_XHP_CLOSE_TAG 
     { Xhp ($1, $2, $3, $4, $5)  }
 | T_XHP_OPEN_TAG xhp_attributes T_XHP_SLASH_GT
     { XhpSingleton ($1, $2, $3) }

xhp_child:
 | T_XHP_TEXT           { XhpText $1 }
 | xhp_html             { XhpNested $1 }
 | TOBRACE expr TCBRACE { XhpExpr ($1, $2, $3) }

xhp_attribute:
 | T_XHP_ATTR TEQ xhp_attribute_value { $1, $2, $3 }

xhp_attribute_value:
 | TGUIL encaps_list TGUIL { XhpAttrString ($1, $2, $3) }
 | TOBRACE expr TCBRACE    { XhpAttrExpr ($1, $2, $3) }

 /*(* ugly: one cannot use T_IDENT here, because the lexer is still in
    * XHP mode which means every ident is transformed in a xhp attribute
    *)*/
 /*(* sgrep_ext: *)*/
 | T_XHP_ATTR { sgrep_guard (SgrepXhpAttrValueMvar ($1)) }

/*(*----------------------------*)*/
/*(*2 auxillary bis *)*/
/*(*----------------------------*)*/

exit_expr:
 | /*(*empty*)*/	{ None }
 | TOPAR TCPAR		{ Some($1, None, $2) }
 | TOPAR expr TCPAR	{ Some($1, Some $2, $3) }

/*(*e: GRAMMAR variable *)*/
/*(*e: GRAMMAR expression *)*/

/*(*************************************************************************)*/
/*(*1 Ident, namespace *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR namespace *)*/

ident: 
 | T_IDENT { $1 }

/*(* xhp: it is ok to use XHP keywords in place where regular PHP names
   * are expected as in 'function children($x) { ... }'.
   * 
   * We extend here the grammar to support those "overloading". An 
   * alternative would be to extend the lexer to only lex XHP keywords 
   * in certain context, but this would force to share some states between
   * the lexer and parser.
   * 
   * todo? emit a warning when the user use XHP keywords for regular idents ?
   *)*/
 | T_XHP_ATTRIBUTE { Ast.str_of_info $1, $1 }
 | T_XHP_CATEGORY  { Ast.str_of_info $1, $1 }
 | T_XHP_CHILDREN  { Ast.str_of_info $1, $1 }

 | T_XHP_ENUM  { Ast.str_of_info $1, $1 }
 | T_XHP_ANY  { Ast.str_of_info $1, $1 }
 | T_XHP_PCDATA  { Ast.str_of_info $1, $1 }

/*
(* todo? Maybe we should allow 'static' here and also any kind of
 * variable. Right now each time we use 'qualifier' in some
 * rules we have to copy the rule to also allow static:: and
 * even $foo::
 *)*/
qualifier: class_name_or_selfparent TCOLCOL { $1, $2 }

class_name_or_selfparent:
 | fully_qualified_class_name { ClassName $1 }
 | T_SELF   { Self $1 }
 | T_PARENT { Parent $1 }
/*(* php 5.3 late static binding *)*/
 | T_STATIC { LateStatic $1 }

fully_qualified_class_name: 
 | ident { Name $1 }
 /*(*s: fully_qualified_class_name grammar rule hook *)*/
  /*(* xhp: an XHP element use *)*/
  | T_XHP_COLONID_DEF { XhpName $1 }
 /*(*e: fully_qualified_class_name grammar rule hook *)*/

/*(* todo? no support for namespace for now *)*/
class_namespace_string:
  | ident { Name $1 }

/*(*e: GRAMMAR namespace *)*/

variable_class_name: reference_variable { $1 }

/*(*************************************************************************)*/
/*(*1 Class bis *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR class bis *)*/
class_name_reference:
 | class_name_or_selfparent	{ ClassNameRefStatic $1 }
 | dynamic_class_name_reference	{ ClassNameRefDynamic (fst $1, snd $1) }

dynamic_class_name_reference:
 | base_variable_bis { ($1, []) }
 | base_variable_bis 
     T_OBJECT_OPERATOR object_property 
     dynamic_class_name_variable_properties
     { ($1, ($2, $3)::$4) }


base_variable_bis: base_variable { basevar_to_variable $1 }

method_or_not:
  | TOPAR function_call_parameter_list TCPAR    { Some ($1, $2, $3) }
  | /*(*empty*)*/ { None }

ctor_arguments:
  | TOPAR function_call_parameter_list TCPAR	{ Some ($1, $2, $3) }
  | /*(*empty*)*/ { None }
/*(*x: GRAMMAR class bis *)*/
/*(*----------------------------*)*/
/*(*2 object property, variable property *)*/
/*(*----------------------------*)*/

object_property:
 | object_dim_list          { ObjProp $1 }
 | variable_without_objects_bis { ObjPropVar $1 }

variable_without_objects_bis: variable_without_objects 
  { vwithoutobj_to_variable $1 }

/*(* quite similar to reference_variable, but without the '$' *)*/
object_dim_list:
 | variable_name { $1 }
 | object_dim_list TOBRA dim_offset TCBRA	{ OArrayAccess($1, ($2,$3,$4)) }
 | object_dim_list TOBRACE expr TCBRACE		{ OBraceAccess($1, ($2,$3,$4)) }

variable_name:
 | T_IDENT		{ OName (Name $1) }
 | TOBRACE expr TCBRACE	{ OBrace ($1,$2,$3) }


variable_property: T_OBJECT_OPERATOR object_property method_or_not
  { $1, $2, $3 }

dynamic_class_name_variable_property: T_OBJECT_OPERATOR object_property
  { $1, $2 }

/*(*e: GRAMMAR class bis *)*/
/*(*************************************************************************)*/
/*(*1 Encaps *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR encaps *)*/
encaps:
 | T_ENCAPSED_AND_WHITESPACE { EncapsString $1 }
 | T_VARIABLE                                   
     { let refvar = (Var2 (DName $1, Ast.noScope())) in
       let basevar = None, ([], refvar) in
       let basevarbis = BaseVar basevar in
       let var = Variable (basevarbis, []) in
       EncapsVar (variable2_to_lvalue var)
     }
 | T_VARIABLE TOBRA encaps_var_offset TCBRA	
     { let refvar = (Var2 (DName $1, Ast.noScope())) in
       let dimoffset = Some (mk_e $3) in
       let refvar = VArrayAccess2(refvar, ($2, dimoffset, $4)) in
       let basevar = None, ([], refvar) in
       let basevarbis = BaseVar basevar in
       let var = Variable (basevarbis, []) in
       EncapsVar (variable2_to_lvalue var)
     }
 | T_VARIABLE T_OBJECT_OPERATOR T_IDENT        
     { let refvar = (Var2 (DName $1, Ast.noScope())) in
       let basevar = None, ([], refvar) in
       let basevarbis = BaseVar basevar in
       let prop_string = ObjProp (OName (Name $1)) in
       let obj_prop = ($2, prop_string, None) in
       let var = Variable (basevarbis, [obj_prop]) in
       EncapsVar (variable2_to_lvalue var)
     }

 /*(* for ${beer}s. Note that this rule does not exist in the original PHP
    * grammar. Instead only the case with a TOBRA after the T_STRING_VARNAME
    * is covered. The case with only a T_STRING_VARNAME is handled 
    * originally in the scalar rule, but it does not makes sense to me
    * as it's really more a variable than a scaler. So for now I have
    * defined this rule. maybe it's too restrictive, we'll see.
    *)*/
 | T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME TCBRACE 
     { 
       (* this is not really a T_VARIABLE, bit it's still conceptually
        * a variable so we build it almost like above
        *)
       let refvar = (Var2 (DName $2, Ast.noScope())) in
       let basevar = None, ([], refvar) in
       let basevarbis = BaseVar basevar in
       let var = Variable (basevarbis, []) in
       EncapsDollarCurly ($1, variable2_to_lvalue var, $3)
     }

 | T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME  TOBRA expr TCBRA  TCBRACE 
     { let refvar = (Var2 (DName $2, Ast.noScope())) in
       let dimoffset = Some ($4) in
       let refvar = VArrayAccess2(refvar, ($3, dimoffset, $5)) in
       
       let basevar = None, ([], refvar) in
       let basevarbis = BaseVar basevar in
       let var = Variable (basevarbis, []) in
       EncapsDollarCurly ($1, variable2_to_lvalue var, $6)
     }

 /*(* for {$beer}s *)*/
 | T_CURLY_OPEN variable TCBRACE           { EncapsCurly($1, $2, $3) }
 /*(* for ? *)*/
 | T_DOLLAR_OPEN_CURLY_BRACES expr TCBRACE { EncapsExpr ($1, $2, $3) }
/*(*x: GRAMMAR encaps *)*/
encaps_var_offset:
 | T_IDENT	{ 
     (* It looks like an ident but as we are in encaps_var_offset, 
      * php allows array access inside strings to omit the quote
      * around fieldname, so it's actually really a Constant (String)
      * rather than an ident, as we usually do for other T_IDENT
      * cases.
      *)
     let cst = String $1 in (* will not have enclosing "'"  as usual *)
     Sc (C cst)
   }
 | T_VARIABLE	{ 
       let refvar = (Var2 (DName $1, Ast.noScope())) in
       let basevar = None, ([], refvar) in
       let basevarbis = BaseVar basevar in
       let var = Variable (basevarbis, []) in
       Lv (variable2_to_lvalue var)
   }
 | T_NUM_STRING	{
     (* the original php lexer does not return some numbers for 
      * offset of array access inside strings. Not sure why ...
      * TODO?
      *)
     let cst = String $1 in (* will not have enclosing "'"  as usual *)
     Sc (C cst)
   }
/*(*e: GRAMMAR encaps *)*/
/*(*************************************************************************)*/
/*(*1 xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/
/*(*s: GRAMMAR xxxlist or xxxopt *)*/
top_statement_list:
 | top_statement_list  top_statement { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

/*(*s: repetitive xxx_list *)*/
inner_statement_list:
 | inner_statement_list  inner_statement { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

class_statement_list:
 | class_statement_list class_statement { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

encaps_list:
 | encaps_list encaps                  { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

variable_properties:
 | variable_properties variable_property { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

dynamic_class_name_variable_properties:
 | dynamic_class_name_variable_properties dynamic_class_name_variable_property 
     { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

xhp_attributes:
 | xhp_attributes xhp_attribute { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

xhp_children:
 | xhp_children xhp_child { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }

trait_rules:
 | trait_rules trait_rule { $1 ++ [$2] }
 | /*(*empty*)*/ { [] }
/*(*e: repetitive xxx_list *)*/

additional_catches:
 | non_empty_additional_catches { $1 }
 | /*(*empty*)*/ { [] }

non_empty_additional_catches:
 | additional_catch                              { [$1] }
 | non_empty_additional_catches additional_catch { $1 ++ [$2] }

/*(*s: repetitive xxx and non_empty_xxx *)*/
method_modifiers:
 | /*(*empty*)*/				{ [] }
 | non_empty_member_modifiers			{ $1 } 

non_empty_member_modifiers:
 | member_modifier				{ [$1] }
 | non_empty_member_modifiers member_modifier	{ $1 ++ [$2] }


parameter_list:
 | non_empty_parameter_list   { $1 }
 | /*(*empty*)*/              { [] }

function_call_parameter_list:
 | non_empty_function_call_parameter_list      { $1 }
 | /*(*empty*)*/			       { [] }
/*(*e: repetitive xxx and non_empty_xxx *)*/

unset_variables:
 | unset_variable { [Left $1] }
 | unset_variables TCOMMA unset_variable { $1 ++ [Right $2; Left $3] }

/*(*s: repetitive xxx_list with TCOMMA *)*/
global_var_list:
 | global_var				{ [Left $1] }
 | global_var_list TCOMMA global_var	{ $1 ++ [Right $2; Left $3] }

echo_expr_list:
 | expr				   { [Left $1] }
 | echo_expr_list TCOMMA expr      { $1 ++ [Right $2; Left $3] }

assignment_list:
 | assignment_list_element                        { [Left $1] }
 | assignment_list TCOMMA assignment_list_element { $1 ++ [Right $2; Left $3] }

isset_variables:
 | variable 			       { [Left $1] }
 | isset_variables TCOMMA variable     { $1 ++ [Right $2; Left $3] }

interface_list:
 | fully_qualified_class_name			    { [Left $1] }
 | interface_list TCOMMA fully_qualified_class_name { $1 ++ [Right $2; Left $3]}

trait_list:
 | fully_qualified_class_name			{ [Left $1] }
 | trait_list TCOMMA fully_qualified_class_name { $1 ++ [Right $2; Left $3] }


declare_list:
 | declare                    	{ [Left $1] }
 | declare_list TCOMMA declare	{ $1 ++ [Right $2; Left $3] }

non_empty_for_expr:
  | expr      			     { [Left $1] }
  | non_empty_for_expr TCOMMA	expr { $1 ++ [Right $2; Left $3] }

xhp_attribute_decls:
 | xhp_attribute_decl { [Left $1] }
 | xhp_attribute_decls TCOMMA xhp_attribute_decl { $1 ++ [Right $2; Left $3] }

xhp_enum_list:
 | xhp_enum { [Left $1] }
 | xhp_enum_list TCOMMA xhp_enum { $1 ++ [Right $2; Left $3] }

xhp_category_list:
 | xhp_category { [Left $1] }
 | xhp_category_list TCOMMA xhp_category { $1 ++ [Right $2; Left $3] }

/*(*e: repetitive xxx_list with TCOMMA *)*/
possible_comma:
 | /*(*empty*)*/ { [] }
 | TCOMMA        { [Right $1] }

possible_comma2:
 | /*(*empty*)*/ { [] }
 | TCOMMA        { [Right $1] }

static_array_pair_list_rev:
 | /*(*empty*)*/ {  [] }
 | non_empty_static_array_pair_list_rev possible_comma	{ $2++$1 }

array_pair_list_rev:
 | /*(*empty*)*/ { [] }
 | non_empty_array_pair_list_rev possible_comma2	{ $2++$1 }

/*(*e: GRAMMAR xxxlist or xxxopt *)*/

/*(*e: GRAMMAR long set of rules *)*/
/*(*e: parser_php.mly *)*/
