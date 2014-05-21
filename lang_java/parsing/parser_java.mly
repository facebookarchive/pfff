/*
(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License
 *
 * LALR(1) (ocamlyacc) grammar for Java
 *
 * Attempts to conform to:
 * The Java Language Specification, Second Edition
 * - James Gosling, Bill Joy, Guy Steele, Gilad Bracha
 *
 * Many modifications by Yoann Padioleau. Attempts to conform to:
 * The Java Language Specification, Third Edition, with some fixes from
 * http://www.cmis.brighton.ac.uk/staff/rnb/bosware/javaSyntax/syntaxV2.html
 *
 * Support for:
 *  - generics
 *  - enums, foreach, ...
 *  - annotations (partial)
 *)
 */
%{
open Common
open Ast_java

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo? use a Ast.special? *)
let this_ident ii = [], ("this", ii)
let super_ident ii = [], ("super", ii)
let super_identifier ii = ("super", ii)

let named_type (str, ii) = TBasic (str,ii)
let void_type ii = named_type ("void", ii)

(* we have to use a 'name' to specify reference types in the grammar
 * because of some ambiguity but what we really wanted was an
 * identifier followed by some type arguments.
 *)
let (class_type: name_or_class_type -> class_type) = fun xs ->
  xs +> List.map (function
  | Id x -> x, []
  | Id_then_TypeArgs (x, xs) -> x, xs
  | TypeArgs_then_Id _ -> raise Parsing.Parse_error
  )

let (name: name_or_class_type -> name) = fun xs ->
  xs +> List.map (function
  | Id x -> [], x
  | Id_then_TypeArgs (x, xs) ->
      (* this is ok because of the ugly trick we do for Cast
       * where we transform a Name into a ref_type
       *)
      xs, x
  | TypeArgs_then_Id (xs, Id x) ->
      xs, x
  | TypeArgs_then_Id (_xs, _) ->
      raise Parsing.Parse_error
  )

let (qualified_ident: name_or_class_type -> qualified_ident) = fun xs ->
  xs +> List.map (function
  | Id x -> x
  | Id_then_TypeArgs _ -> raise Parsing.Parse_error
  | TypeArgs_then_Id _ -> raise Parsing.Parse_error
  )

type var_decl_id =
  | IdentDecl of ident
  | ArrayDecl of var_decl_id

(* Move array dimensions from variable name to type. *)
let rec canon_var mods t v =
  match v with
  | IdentDecl str -> { v_mods = mods; v_type = t; v_name = str }
  | ArrayDecl v' -> canon_var mods (TArray t) v'

let method_header mods mtype (v, formals) throws =
  { m_var = canon_var mods mtype v; m_formals = formals;
    m_throws = throws; m_body = Empty }

(* Return a list of field declarations in canonical form. *)

let decls f = fun mods vtype vars ->
  let dcl (v, init) =
    f { f_var = canon_var mods vtype v; f_init = init }
  in
  List.map dcl vars

let constructor_invocation name args =
  Expr (Call ((Name name), args))

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 The comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* Those tokens are not even used in this file because they are
   * filtered in some intermediate phases (in Parse_java.lexer_function
   * by using TH.is_comment(). But they still must be declared
   * because ocamllex may generate them, or some intermediate phases may also
   * generate them (like some functions in parsing_hacks.ml).
   *)*/
%token <Parse_info.info> TComment TCommentNewline TCommentSpace

/*(*-----------------------------------------*)*/
/*(*2 The normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with "values" *)*/
%token <string * Parse_info.info> TInt
%token <string * Parse_info.info> TFloat
%token <string * Parse_info.info> TChar
%token <string * Parse_info.info> TString

%token <(string * Parse_info.info)> LITERAL

%token <(string * Parse_info.info)> IDENTIFIER
%token <(string * Parse_info.info)> PRIMITIVE_TYPE

/*
 * 3.11 Separators
 */
%token <Parse_info.info> LP		/* ( */
%token <Parse_info.info> RP		/* ) */
%token <Parse_info.info> LC		/* { */
%token <Parse_info.info> RC		/* } */
%token <Parse_info.info> LB		/* [ */
%token <Parse_info.info> RB		/* ] */
%token <Parse_info.info> SM		/* ; */
%token <Parse_info.info> CM		/* , */
%token <Parse_info.info> DOT		/* . */

/*
 * 3.12 Operators
 */
%token <Parse_info.info> EQ		/* = */
%token <Parse_info.info> GT		/* > */
%token <Parse_info.info> LT		/* < */
%token <Parse_info.info> NOT		/* ! */
%token <Parse_info.info> COMPL		/* ~ */
%token <Parse_info.info> COND		/* ? */
%token <Parse_info.info> COLON		/* : */
%token <Parse_info.info> EQ_EQ		/* == */
%token <Parse_info.info> LE		/* <= */
%token <Parse_info.info> GE		/* >= */
%token <Parse_info.info> NOT_EQ		/* != */
%token <Parse_info.info> AND_AND		/* && */
%token <Parse_info.info> OR_OR		/* || */
%token <Parse_info.info> INCR		/* ++ */
%token <Parse_info.info> DECR		/* -- */
%token <Parse_info.info> PLUS		/* + */
%token <Parse_info.info> MINUS		/* - */
%token <Parse_info.info> TIMES		/* * */
%token <Parse_info.info> DIV		/* / */
%token <Parse_info.info> AND		/* & */
%token <Parse_info.info> OR		/* | */
%token <Parse_info.info> XOR		/* ^ */
%token <Parse_info.info> MOD		/* % */
%token <Parse_info.info> LS		/* << */
%token <Parse_info.info> SRS		/* >> */
%token <Parse_info.info> URS		/* >>> */

%token <Parse_info.info> AT		/* @ */
%token <Parse_info.info> DOTS		/* ... */

/*(* to avoid some conflicts *)*/
%token <Parse_info.info> LB_RB

/*(* Those fresh tokens are created in parsing_hacks_java.ml *)*/
%token <Parse_info.info> LT2		/* < */

%token <(string * Parse_info.info)> OPERATOR_EQ	/* += -= *= /= &= |= ^= %= <<= >>= >>>= */

/*(* keywords tokens *)*/
/*
 * 3.9 Keywords
 */
%token <Parse_info.info>
 ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE
 DEFAULT DO DOUBLE ELSE EXTENDS FINAL FINALLY FLOAT FOR GOTO
 IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG
 NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN
 SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
 THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE
 /*(* javaext: *)*/
 ASSERT
 ENUM

/*(*-----------------------------------------*)*/
/*(*2 Extra tokens: *)*/
/*(*-----------------------------------------*)*/

/*(* classic *)*/
%token <Parse_info.info> TUnknown
%token <Parse_info.info> EOF

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/
/*
(*
 * The start production must begin with a lowercase letter,
 * because ocamlyacc defines the parsing function with that name.
 *)
*/
%start goal
%type <Ast_java.program> goal

%%

/*(*************************************************************************)*/
/*(*1 TOC *)*/
/*(*************************************************************************)*/
/*
(* TOC:
 *  goal
 *  name
 *  type
 *  expr
 *  statement
 *  declaration
 *  anotation
 *  class/interfaces
 *)
*/
/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/

goal: compilation_unit EOF  { $1 }

/* 7.3 */
compilation_unit:
  package_declaration_opt import_declarations_opt type_declarations_opt
  { { package = $1; imports = $2; decls = $3; } }

/*(*************************************************************************)*/
/*(*1 Package, Import, Type *)*/
/*(*************************************************************************)*/

/* 7.4.1 */
package_declaration: PACKAGE name SM  { qualified_ident $2 }

/* 7.5 */
import_declaration:
 | IMPORT static_opt name SM            { $2, qualified_ident $3 }
 | IMPORT static_opt name DOT TIMES SM  { $2, (qualified_ident $3 @["*", $5])}

/* 7.6 */
type_declaration:
 | class_declaration      { [Class $1] }
 | interface_declaration  { [Class $1] }
 | enum_declaration            { [Enum $1] }
 | annotation_type_declaration { ast_todo }
 | SM  { [] }

/*(*************************************************************************)*/
/*(*1 Ident, namespace  *)*/
/*(*************************************************************************)*/
/* 3.8 */
identifier: IDENTIFIER { $1 }

/* 6.5 */
name:
 | identifier_           { [$1] }
 | name DOT identifier_  { $1 @ [$3] }
 | name DOT LT2 type_arguments GT identifier_ { $1@[TypeArgs_then_Id($4,$6)] }

identifier_:
 | identifier                       { Id $1 }
 | identifier LT2 type_arguments GT { Id_then_TypeArgs($1, $3) }

/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/* 4.1 */
type_java:
 | primitive_type           { $1 }
 | class_or_interface_type  { $1 }
 | array_type               { $1 }

/* 4.2 */
primitive_type: PRIMITIVE_TYPE  { named_type $1 }

class_or_interface_type: name { TClass (class_type $1) }

/* 4.3 */
reference_type:
 | class_or_interface_type { $1 }
 | array_type { $1 }

array_type:
 | primitive_type          LB_RB { TArray $1 }
 | class_or_interface_type LB_RB { TArray $1 }
 | array_type              LB_RB { TArray $1 }

/*(*----------------------------*)*/
/*(*2 Generics arguments *)*/
/*(*----------------------------*)*/

type_argument:
 | reference_type { TArgument $1 }
 | COND           { TQuestion None }
 | COND EXTENDS reference_type { TQuestion (Some (false, $3)) }
 | COND SUPER   reference_type { TQuestion (Some (true, $3))}

/*(*----------------------------*)*/
/*(*2 Generics parameters *)*/
/*(*----------------------------*)*/
type_parameter:
 | identifier               { TParam ($1, []) }
 | identifier EXTENDS bound { TParam ($1, $3) }

bound: ref_type_and_list { $1 }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

/* 15.8 */
primary:
 | primary_no_new_array       { $1 }
 | array_creation_expression  { $1 }

primary_no_new_array:
 | literal             { $1 }
 | class_literal       { $1 }
 | THIS                { Name [this_ident $1] }
 | name DOT THIS       { Name (name $1 @ [this_ident $3]) }
 | LP expression RP    { $2 }
 | class_instance_creation_expression { $1 }
 | field_access                       { $1 }
 | method_invocation                  { $1 }
 | array_access                       { $1 }

/* 3.10 */
literal:
 | LITERAL { Literal ($1) }

 | TInt    { Literal ($1) }
 | TFloat  { Literal ($1) }
 | TChar   { Literal ($1) }
 | TString { Literal ($1) }

/* 15.8.2 */
class_literal:
 | primitive_type DOT CLASS  { ClassLiteral $1 }
 | name           DOT CLASS  { ClassLiteral (TClass (class_type ($1))) }
 | array_type     DOT CLASS  { ClassLiteral $1 }
 | VOID           DOT CLASS  { ClassLiteral (void_type $1) }

/* 15.9 */
class_instance_creation_expression:
 | NEW name LP argument_list_opt RP class_body_opt
       { NewClass (TClass (class_type $2), $4, $6) }
 | primary DOT NEW identifier LP argument_list_opt RP class_body_opt
       { NewQualifiedClass ($1, $4, $6, $8) }
 /*(* not in 2nd edition java language specification. *)*/
 | name DOT NEW identifier LP argument_list_opt RP class_body_opt
       { NewQualifiedClass ((Name (name $1)), $4, $6, $8) }

/* 15.10 */
array_creation_expression:
 | NEW primitive_type dim_exprs dims_opt
       { NewArray ($2, List.rev $3, $4, None) }
 | NEW name dim_exprs dims_opt
       { NewArray (TClass (class_type ($2)), List.rev $3, $4, None) }
 | NEW primitive_type dims array_initializer
       { NewArray ($2, [], $3, Some $4) }
 | NEW name dims array_initializer
       { NewArray (TClass (class_type ($2)), [], $3, Some $4) }

dim_expr: LB expression RB  { $2 }

dims:
 | LB_RB       { 1 }
 | dims LB_RB  { $1 + 1 }

/* 15.11 */
field_access:
 | primary DOT identifier        { Dot ($1, $3) }
 | SUPER   DOT identifier        { Dot (Name [super_ident $1], $3) }
 | name DOT SUPER DOT identifier { Dot (Name (name $1@[super_ident $3]), $5) }

/* 15.13 */
array_access:
 | name LB expression RB                  { ArrayAccess ((Name (name $1)), $3)}
 | primary_no_new_array LB expression RB  { ArrayAccess ($1, $3) }

/*(*----------------------------*)*/
/*(*2 Method call *)*/
/*(*----------------------------*)*/

/* 15.12 */
method_invocation:
 | name LP argument_list_opt RP
        {
          match List.rev $1 with
          (* TODO: lose information of TypeArgs_then_Id *)
          | ((Id x) | (TypeArgs_then_Id (_, Id x)))::xs ->
              let (xs: identifier_ list) =
                (match xs with
                (* should be a "this" or "self" *)
                | [] -> [Id ("this", fakeInfo "this")]
                | _ -> List.rev xs
                )
              in
              Call (Dot (Name (name (xs)), x), $3)
          | _ ->
              pr2 "method_invocation pb";
              pr2_gen $1;
              raise Impossible
        }
 | primary DOT identifier LP argument_list_opt RP
	{ Call ((Dot ($1, $3)), $5) }
 | SUPER DOT identifier LP argument_list_opt RP
	{ Call ((Dot (Name [super_ident $1], $3)), $5) }
 | name DOT SUPER DOT identifier LP argument_list_opt RP
	{ Call (Dot (Name (name $1 @ [super_ident $3]), $5), $7)}

/*(*----------------------------*)*/
/*(*2 Arithmetic *)*/
/*(*----------------------------*)*/

/* 15.14 */
postfix_expression:
 | primary  { $1 }
 | name     {
     (* Ambiguity. It could be a field access (Dot) or a qualified
      * name (Name). See ast_java.ml note on the Dot constructor for
      * more information.
      * The last dot has to be a Dot and not a Name at least,
      * but more elements of Name could be a Dot too.
      *)
     match List.rev $1 with
     | (Id id)::x::xs ->
         Dot (Name (name (List.rev (x::xs))), id)
     | _ ->
         Name (name $1)
   }

 | post_increment_expression  { $1 }
 | post_decrement_expression  { $1 }

/* 15.14.1 */
post_increment_expression: postfix_expression INCR  { Postfix ($1, "++") }
/* 15.14.2 */
post_decrement_expression: postfix_expression DECR  { Postfix ($1, "--") }

/* 15.15 */
unary_expression:
 | pre_increment_expression  { $1 }
 | pre_decrement_expression  { $1 }
 | PLUS unary_expression  { Prefix ("+", $2) }
 | MINUS unary_expression  { Prefix ("-", $2) }
 | unary_expression_not_plus_minus  { $1 }

pre_increment_expression: INCR unary_expression  { Prefix ("++", $2) }

pre_decrement_expression: DECR unary_expression  { Prefix ("--", $2) }

unary_expression_not_plus_minus:
 | postfix_expression  { $1 }
 | COMPL unary_expression  { Prefix ("~", $2) }
 | NOT unary_expression    { Prefix ("!", $2) }
 | cast_expression  { $1 }

/* 15.16 */
/*
(*
 * original rule:
 * | LP primitive_type dims_opt RP unary_expression
 * | LP reference_type RP unary_expression_not_plus_minus
 * Semantic action must ensure that '( expression )' is really '( name )'.
 * Conflict with regular paren expr; when see ')' dont know if
 * can reduce to expr or shift name, so have to use
 * expr in both cases.
 *)*/
cast_expression:
 | LP primitive_type RP unary_expression  { Cast ($2, $4) }
 | LP expression RP unary_expression_not_plus_minus
	{
          let typname =
            match $2 with
            | Name name ->
                TClass (name +> List.map (fun (xs, id) -> id, xs))
            (* ugly, undo what was done in postfix_expression *)
            | Dot (Name name, id) ->
                TClass ((name @ [[], id]) +> List.map (fun (xs, id) -> id, xs))
            | _ ->
                pr2 "cast_expression pb";
                pr2_gen $2;
                raise Todo
          in
          Cast (typname, $4)
        }
 | LP array_type RP unary_expression_not_plus_minus  { Cast ($2, $4) }

/* 15.17 */
multiplicative_expression:
 | unary_expression  { $1 }
 | multiplicative_expression TIMES unary_expression { Infix ($1, "*", $3) }
 | multiplicative_expression DIV unary_expression   { Infix ($1, "/", $3) }
 | multiplicative_expression MOD unary_expression   { Infix ($1, "%", $3) }


/* 15.18 */
additive_expression:
 | multiplicative_expression  { $1 }
 | additive_expression PLUS multiplicative_expression { Infix ($1, "+", $3) }
 | additive_expression MINUS multiplicative_expression { Infix ($1, "-", $3) }


/* 15.19 */
shift_expression:
 | additive_expression  { $1 }
 | shift_expression LS additive_expression  { Infix ($1, "<<", $3) }
 | shift_expression SRS additive_expression  { Infix ($1, ">>", $3) }
 | shift_expression URS additive_expression  { Infix ($1, ">>>", $3) }


/* 15.20 */
relational_expression:
 | shift_expression  { $1 }
 /*(* possible many conflicts if don't use a LT2 *)*/
 | relational_expression LT shift_expression  { Infix ($1, "<", $3) }
 | relational_expression GT shift_expression  { Infix ($1, ">", $3) }
 | relational_expression LE shift_expression  { Infix ($1, "<=", $3) }
 | relational_expression GE shift_expression  { Infix ($1, ">=", $3) }
 | relational_expression INSTANCEOF reference_type  { InstanceOf ($1, $3) }


/* 15.21 */
equality_expression:
 | relational_expression  { $1 }
 | equality_expression EQ_EQ relational_expression  { Infix ($1, "==", $3) }
 | equality_expression NOT_EQ relational_expression { Infix ($1, "!=", $3) }


/* 15.22 */
and_expression:
 | equality_expression  { $1 }
 | and_expression AND equality_expression  { Infix ($1, "&", $3) }

exclusive_or_expression:
 | and_expression  { $1 }
 | exclusive_or_expression XOR and_expression  { Infix ($1, "^", $3) }


inclusive_or_expression:
 | exclusive_or_expression  { $1 }
 | inclusive_or_expression OR exclusive_or_expression  { Infix ($1, "|", $3) }

/* 15.23 */
conditional_and_expression:
 | inclusive_or_expression  { $1 }
 | conditional_and_expression AND_AND inclusive_or_expression
     { Infix($1,"&&",$3) }


/* 15.24 */
conditional_or_expression:
 | conditional_and_expression  { $1 }
 | conditional_or_expression OR_OR conditional_and_expression
     { Infix ($1, "||", $3) }

/*(*----------------------------*)*/
/*(*2 Ternary *)*/
/*(*----------------------------*)*/

/* 15.25 */
conditional_expression:
 | conditional_or_expression
     { $1 }
 | conditional_or_expression COND expression COLON conditional_expression
     { Conditional ($1, $3, $5) }

/*(*----------------------------*)*/
/*(*2 Assign *)*/
/*(*----------------------------*)*/

/* 15.26 */
assignment_expression:
 | conditional_expression  { $1 }
 | assignment              { $1 }

assignment: left_hand_side assignment_operator assignment_expression
    { Assignment ($1, fst $2, $3) }


left_hand_side:
 | name          { Name (name $1) }
 | field_access  { $1 }
 | array_access  { $1 }

assignment_operator:
 | EQ  { "=", $1  }
 | OPERATOR_EQ  { $1 }


/*(*----------------------------*)*/
/*(*2 Shortcuts *)*/
/*(*----------------------------*)*/
/* 15.27 */
expression: assignment_expression  { $1 }

/* 15.28 */
constant_expression: expression  { $1 }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

/* 14.5 */
statement:
 | statement_without_trailing_substatement  { $1 }

 | labeled_statement  { $1 }
 | if_then_statement  { $1 }
 | if_then_else_statement  { $1 }
 | while_statement  { $1 }
 | for_statement  { $1 }

statement_without_trailing_substatement:
 | block  { $1 }
 | empty_statement  { $1 }
 | expression_statement  { $1 }
 | switch_statement  { $1 }
 | do_statement  { $1 }
 | break_statement  { $1 }
 | continue_statement  { $1 }
 | return_statement  { $1 }
 | synchronized_statement  { $1 }
 | throw_statement  { $1 }
 | try_statement  { $1 }
 /*(* javaext:  *)*/
 | ASSERT expression SM                  { Assert ($2, None) }
 | ASSERT expression COLON expression SM { Assert ($2, Some $4) }

/* 14.2 */
block: LC block_statements_opt RC  { Block $2 }

block_statement:
 | local_variable_declaration_statement  { $1 }
 | class_declaration  { [LocalClass $1] }
 | statement          { [$1] }

/* 14.4 */
local_variable_declaration_statement: local_variable_declaration SM
 { List.map (fun x -> LocalVar x) $1 }

/*(* cant factorize with variable_modifier_opt, conflicts otherwise *)*/
local_variable_declaration:
 | type_java variable_declarators
     { decls (fun x -> x) [] $1 (List.rev $2) }
 /*(* actually should be variable_modifiers but conflict *)*/
 | modifiers type_java variable_declarators
     { decls (fun x -> x) $1 $2 (List.rev $3) }

/* 14.6 */
empty_statement: SM { Empty }

/* 14.7 */
labeled_statement: identifier COLON statement
   { Label ($1, $3) }

/* 14.8 */
expression_statement: statement_expression SM  { Expr $1 }

/*(* pad: good *)*/
statement_expression:
 | assignment  { $1 }
 | pre_increment_expression  { $1 }
 | pre_decrement_expression  { $1 }
 | post_increment_expression  { $1 }
 | post_decrement_expression  { $1 }
 | method_invocation  { $1 }
 | class_instance_creation_expression  { $1 }



/* 14.9 */
if_then_statement: IF LP expression RP statement
   { If ($3, $5, Empty) }

if_then_else_statement: IF LP expression RP statement_no_short_if ELSE statement
   { If ($3, $5, $7) }

/* 14.10 */
switch_statement: SWITCH LP expression RP switch_block
    { Switch ($3, $5) }

switch_block:
 | LC RC  { [] }
 | LC switch_labels RC  { [$2, []] }
 | LC switch_block_statement_groups RC  { List.rev $2 }
 | LC switch_block_statement_groups switch_labels RC
     { List.rev ((List.rev $3, []) :: $2) }

switch_block_statement_group: switch_labels block_statements  {List.rev $1, $2}

switch_label:
 | CASE constant_expression COLON  { Case $2 }
 | DEFAULT COLON                   { Default }


/* 14.11 */
while_statement: WHILE LP expression RP statement
     { While ($3, $5) }

/* 14.12 */
do_statement: DO statement WHILE LP expression RP SM
     { Do ($2, $5) }

/*(*----------------------------*)*/
/*(*2 For *)*/
/*(*----------------------------*)*/

/* 14.13 */
for_statement:
  FOR LP for_control RP statement
	{ For ($3, $5) }

for_control:
 | for_init_opt SM expression_opt SM for_update_opt
     { ForClassic ($1, Common2.option_to_list $3, $5) }
 | for_var_control
     { let (a, b) = $1 in Foreach (a, b) }

for_init_opt:
 | /*(*empty*)*/  { ForInitExprs [] }
 | for_init       { $1 }

for_init:
| statement_expression_list   { ForInitExprs $1 }
| local_variable_declaration  { ForInitVars $1 }

for_update: statement_expression_list  { $1 }

for_var_control:
 |           type_java variable_declarator_id for_var_control_rest
     {  canon_var [] $1 $2, $3 }
/*(* actually only FINAL is valid here, but cant because get shift/reduce
   * conflict otherwise because for_init can be a local_variable_decl
   *)*/
 | modifiers type_java variable_declarator_id for_var_control_rest
     { canon_var $1 $2 $3, $4 }

for_var_control_rest: COLON expression { $2 }

/*(*----------------------------*)*/
/*(*2 Other *)*/
/*(*----------------------------*)*/

/* 14.14 */
break_statement: BREAK identifier_opt SM  { Break $2 }
/* 14.15 */
continue_statement: CONTINUE identifier_opt SM  { Continue $2 }
/* 14.16 */
return_statement: RETURN expression_opt SM  { Return $2 }

/* 14.18 */
synchronized_statement: SYNCHRONIZED LP expression RP block { Sync ($3, $5) }

/* 14.17 */
throw_statement: THROW expression SM  { Throw $2 }

/* 14.19 */
try_statement:
 | TRY block catches              { Try ($2, List.rev $3, None) }
 | TRY block catches_opt finally  { Try ($2, $3, Some $4) }

catch_clause:
 | CATCH LP formal_parameter RP block  { $3, $5 }
 /*(* not in 2nd edition java language specification.*) */
 | CATCH LP formal_parameter RP empty_statement  { $3, $5 }

finally: FINALLY block  { $2 }

/*(*----------------------------*)*/
/*(*2 No short if *)*/
/*(*----------------------------*)*/

statement_no_short_if:
 | statement_without_trailing_substatement  { $1 }
 | labeled_statement_no_short_if  { $1 }
 | if_then_else_statement_no_short_if  { $1 }
 | while_statement_no_short_if  { $1 }
 | for_statement_no_short_if  { $1 }

labeled_statement_no_short_if: identifier COLON statement_no_short_if
   { Label ($1, $3) }

if_then_else_statement_no_short_if:
 IF LP expression RP statement_no_short_if ELSE statement_no_short_if
   { If ($3, $5, $7) }

while_statement_no_short_if: WHILE LP expression RP statement_no_short_if
     { While ($3, $5) }

for_statement_no_short_if:
  FOR LP for_control RP statement_no_short_if
	{ For ($3, $5) }

/*(*************************************************************************)*/
/*(*1 Declaration *)*/
/*(*************************************************************************)*/

/* 8.1.1 */
/* 8.3.1 */
/* 8.4.3 */
/* 8.8.3 */
/* 9.1.1 */
/* 9.3 */
/* 9.4 */

/*(*
 * to avoid shift/reduce conflicts, we accept all modifiers
 * in front of all declarations.  the ones not applicable to
 * a particular kind of declaration must be detected in semantic actions.
 *)*/

modifier:
 | PUBLIC       { Public, $1 }
 | PROTECTED    { Protected, $1 }
 | PRIVATE      { Private, $1 }

 | ABSTRACT     { Abstract, $1 }
 | STATIC       { Static, $1 }
 | FINAL        { Final, $1 }

 | STRICTFP     { StrictFP, $1 }
 | TRANSIENT    { Transient, $1 }
 | VOLATILE     { Volatile, $1 }
 | SYNCHRONIZED { Synchronized, $1 }
 | NATIVE       { Native, $1 }

 | annotation { Annotation $1, (info_of_identifier_ (List.hd (List.rev (fst $1)))) }

/*(*************************************************************************)*/
/*(*1 Annotation *)*/
/*(*************************************************************************)*/

annotation:
 | AT name { ($2, None) }
 | AT name LP annotation_element RP { ($2, Some $4) }

annotation_element:
 | /* nothing */ { EmptyAnnotArg }
 | element_value { AnnotArgValue $1 }
 | element_value_pairs { AnnotArgPairInit $1 }

element_value:
 | expr1 { AnnotExprInit $1 }
 | annotation { AnnotNestedAnnot $1 }
 | element_value_array_initializer { AnnotArrayInit $1 }

element_value_pair:
 | identifier EQ element_value { ($1, $3) }


element_value_array_initializer:
 | LC RC { [] }
 | LC element_values RC { $2 }
 | LC element_values CM RC { $2 }

expr1:
 | primary_no_new_array { $1 }
 | primary_no_new_array PLUS primary_no_new_array { failwith "+ inside raw annot values"  }
 | name { NameOrClassType $1 }

/*(*************************************************************************)*/
/*(*1 Class/Interface *)*/
/*(*************************************************************************)*/

/*(*----------------------------*)*/
/*(*2 Class *)*/
/*(*----------------------------*)*/
/* 8.1 */
class_declaration:
 modifiers_opt CLASS identifier type_parameters_opt super_opt interfaces_opt
 class_body
  { { cl_name = $3; cl_kind = ClassRegular;
      cl_mods = $1; cl_tparams = $4;
      cl_extends = $5;  cl_impls = $6;
      cl_body = $7;
     }
  }

/* 8.1.3 */
super: EXTENDS type_java  { $2 }
/* 8.1.4 */
interfaces: IMPLEMENTS ref_type_list  { $2 }

/*(*----------------------------*)*/
/*(*2 Class body *)*/
/*(*----------------------------*)*/
/* 8.1.5 */
class_body: LC class_body_declarations_opt RC  { $2 }

class_body_declaration:
 | class_member_declaration  { $1 }
 | constructor_declaration  { [$1] }

 | instance_initializer  { [$1] }
 | static_initializer  { [$1] }

class_member_declaration:
 | field_declaration  { $1 }
 | method_declaration  { [Method $1] }
 | generic_method_or_constructor_decl { ast_todo }

 | class_declaration  { [Class $1] }
 | interface_declaration  { [Class $1] }
 | enum_declaration { [Enum $1] }
 | annotation_type_declaration { ast_todo }

 | SM  { [] }


/* 8.3 */
field_declaration: modifiers_opt type_java variable_declarators SM
   { decls (fun x -> Field x) $1 $2 (List.rev $3) }


variable_declarator:
 | variable_declarator_id  { $1, None }
 | variable_declarator_id EQ variable_initializer  { $1, Some $3 }


variable_declarator_id:
 | identifier                    { IdentDecl $1 }
 | variable_declarator_id LB_RB  { ArrayDecl $1 }


variable_initializer:
 | expression         { ExprInit $1 }
 | array_initializer  { $1 }

/* 10.6 */
array_initializer:
 | LC comma_opt RC                        { ArrayInit [] }
 | LC variable_initializers comma_opt RC  { ArrayInit (List.rev $2) }


/* 8.4 */
method_declaration: method_header method_body  { { $1 with m_body = $2 } }

method_header:
 | modifiers_opt type_java method_declarator throws_opt
     { method_header $1 $2 $3 $4 }
 | modifiers_opt VOID method_declarator throws_opt
     { method_header $1 (void_type $2) $3 $4 }

method_declarator:
 | identifier LP formal_parameter_list_opt RP  { (IdentDecl $1), $3 }
 | method_declarator LB_RB                     { (ArrayDecl (fst $1)), snd $1 }


generic_method_or_constructor_decl:
  modifiers_opt type_parameters generic_method_or_constructor_rest  { }

generic_method_or_constructor_rest:
 | type_java identifier method_declarator_rest { }
 | VOID identifier method_declarator_rest { }

method_declarator_rest:
 | formal_parameters throws_opt method_body { }

formal_parameters: LP formal_parameter_list_opt RP { $2 }

/* 8.4.4 */
throws: THROWS qualified_ident_list  { $2 }

/* 8.4.5 */
method_body:
 | block  { $1 }
 | SM     { Empty }

/* 8.6 */
instance_initializer: block       { Init (false, $1) }
/* 8.7 */
static_initializer: STATIC block  { Init (true, $2) }

/* 8.8 */
constructor_declaration:
 modifiers_opt constructor_declarator throws_opt constructor_body
  {
    let no_type = TBasic ("void", fakeInfo "void") in
    let (id, formals) = $2 in
    let var = { v_mods = $1; v_type = no_type; v_name = id } in
    Method { m_var = var; m_formals = formals; m_throws = $3;
	     m_body = $4 }
  }

constructor_declarator:	identifier LP formal_parameter_list_opt RP  { $1, $3 }

/* 8.8.5 */
constructor_body:
 | LC block_statements_opt RC                                 { Block $2 }
 | LC explicit_constructor_invocation block_statements_opt RC { Block ($2::$3) }


/* 8.8.5.1 */
explicit_constructor_invocation:
 | THIS LP argument_list_opt RP SM
      { constructor_invocation [this_ident $1] $3 }
 | SUPER LP argument_list_opt RP SM
      { constructor_invocation [super_ident $1] $3 }
 | primary DOT SUPER LP argument_list_opt RP SM
      { Expr (Call ((Dot ($1, super_identifier $3)), $5)) }
 /*(* not in 2nd edition java language specification. *)*/
 | name DOT SUPER LP argument_list_opt RP SM
      { constructor_invocation (name $1 @ [super_ident $3]) $5 }

/*(*----------------------------*)*/
/*(*2 Method parameter *)*/
/*(*----------------------------*)*/

/* 8.4.1 */
formal_parameter: variable_modifiers_opt type_java variable_declarator_id_bis
  { canon_var $1 $2 $3 }

variable_declarator_id_bis:
 | variable_declarator_id      { $1 }
 | DOTS variable_declarator_id { $2 (* todo_ast *) }

variable_modifier:
 | FINAL      { Final, $1 }
 | annotation { (Annotation $1), info_of_identifier_ (List.hd (List.rev (fst $1))) }

/*(*----------------------------*)*/
/*(*2 Interface *)*/
/*(*----------------------------*)*/

/* 9.1 */
interface_declaration:
 modifiers_opt INTERFACE identifier type_parameters_opt  extends_interfaces_opt
 interface_body
  { { cl_name = $3; cl_kind = Interface;
      cl_mods = $1; cl_tparams = $4;
      cl_extends = None; cl_impls = $5;
      cl_body = $6;
    }
  }

/* 9.1.2 */
extends_interfaces:
 | EXTENDS reference_type                { [$2] }
 | extends_interfaces CM reference_type  { $1 @ [$3] }

/*(*----------------------------*)*/
/*(*2 Interface body *)*/
/*(*----------------------------*)*/
/* 9.1.3 */
interface_body:	LC interface_member_declarations_opt RC  { $2 }

interface_member_declaration:
 | constant_declaration  { $1 }
 | abstract_method_declaration  { [Method $1] }
 | interface_generic_method_decl { ast_todo }

 | class_declaration      { [Class $1] }
 | interface_declaration  { [Class $1] }
 | enum_declaration       { [Enum $1] }
 | annotation_type_declaration { ast_todo }
 | SM  { [] }

/* 9.3 */

/*(* note: semicolon is missing in 2nd edition java language specification.*)*/
constant_declaration: modifiers_opt type_java variable_declarators SM
     { decls (fun x -> Field x) $1 $2 (List.rev $3) }

/* 9.4 */
abstract_method_declaration:
 | modifiers_opt type_java method_declarator throws_opt SM
	{ method_header $1 $2 $3 $4 }
 | modifiers_opt VOID method_declarator throws_opt SM
	{ method_header $1 (void_type $2) $3 $4 }

interface_generic_method_decl:
 | modifiers_opt type_parameters type_java identifier interface_method_declator_rest
    { ast_todo }
 | modifiers_opt type_parameters VOID identifier interface_method_declator_rest
    { ast_todo }

interface_method_declator_rest:
 | formal_parameters throws_opt SM { }

/*(*----------------------------*)*/
/*(*2 Enum *)*/
/*(*----------------------------*)*/
enum_declaration: modifiers_opt ENUM identifier interfaces_opt enum_body
   { { en_name = $3; en_mods = $1; en_impls = $4; en_body = $5; } }

/*(* cant factorize in enum_constants_opt comma_opt .... *)*/
enum_body:
 | LC                   enum_body_declarations_opt RC { [], $2 }
 | LC enum_constants    enum_body_declarations_opt RC { $2, $3 }
 | LC enum_constants CM enum_body_declarations_opt RC { $2, $4 }

enum_constant:
 | identifier                         { EnumSimple $1 }
 | identifier LP argument_list_opt RP { EnumConstructor ($1, $3) }
 | identifier LC method_declarations_opt RC  { EnumWithMethods ($1, $3) }

enum_body_declarations: SM class_body_declarations_opt { $2 }

/*(*----------------------------*)*/
/*(*2 Annotation type decl *)*/
/*(*----------------------------*)*/

/*(* cant factorize modifiers_opt *)*/
annotation_type_declaration:
 | modifiers AT INTERFACE identifier annotation_type_body { ast_todo }
 |           AT INTERFACE identifier annotation_type_body { ast_todo }

annotation_type_body: LC annotation_type_element_declarations_opt RC { }

annotation_type_element_declaration:
 annotation_type_element_rest { }

annotation_type_element_rest:
 | modifiers_opt type_java identifier annotation_method_or_constant_rest SM { }

 | class_declaration { }
 | enum_declaration { }
 | interface_declaration { }
 | annotation_type_declaration {  }


annotation_method_or_constant_rest:
 | LP RP { }
 | LP RP DEFAULT element_value { }

annotation_type_element_declarations_opt:
 | { }
 | annotation_type_element_declarations { }

annotation_type_element_declarations:
 | annotation_type_element_declaration { }
 | annotation_type_element_declarations annotation_type_element_declaration { }

/*(*************************************************************************)*/
/*(*1 xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/

import_declarations:
 | import_declaration  { [$1] }
 | import_declarations import_declaration  { $1 @ [$2] }

import_declarations_opt:
 | /*(*empty*)*/  { [] }
 | import_declarations  { $1 }


type_declarations:
 | type_declaration  { $1 }
 | type_declarations type_declaration  { $1 @ $2 }

type_declarations_opt:
 | /*(*empty*)*/  { [] }
 | type_declarations  { $1 }



package_declaration_opt:
 | /*(*empty*)*/  { None }
 | package_declaration  { Some $1 }


modifiers:
 | modifier  { [$1] }
 | modifiers modifier  { $2 :: $1 }

modifiers_opt:
 | /*(*empty*)*/  { [] }
 | modifiers  { List.rev $1 }


super_opt:
 | /*(*empty*)*/  { None }
 | super  { Some $1 }


interfaces_opt:
 | /*(*empty*)*/  { [] }
 | interfaces  { $1 }


ref_type_list:
 | reference_type  { [$1] }
 | ref_type_list CM reference_type  { $1 @ [$3] }

ref_type_and_list:
 | reference_type  { [$1] }
 | ref_type_and_list AND reference_type  { $1 @ [$3] }


class_body_declarations:
 | class_body_declaration  { $1 }
 | class_body_declarations class_body_declaration  { $1 @ $2 }

class_body_declarations_opt:
 | /*(*empty*)*/  { [] }
 | class_body_declarations  { $1 }


variable_declarators:
 | variable_declarator  { [$1] }
 | variable_declarators CM variable_declarator  { $3 :: $1 }


formal_parameter_list:
 | formal_parameter  { [$1] }
 | formal_parameter_list CM formal_parameter  { $3 :: $1 }

formal_parameter_list_opt:
 | /*(*empty*)*/  { [] }
 | formal_parameter_list  { List.rev $1 }


variable_modifiers_opt:
 | /*(*empty*)*/  { [] }
 | variable_modifiers  { $1 }

variable_modifiers:
 | variable_modifier { [$1] }
 | variable_modifiers variable_modifier { $1 @ [$2] }

static_opt:
 | /*(*empty*)*/  { false }
 | STATIC  { true }

throws_opt:
 | /*(*empty*)*/  { [] }
 | throws  { $1 }

qualified_ident_list:
 | name                          { [qualified_ident $1] }
 | qualified_ident_list CM name  { $1 @ [qualified_ident $3] }

extends_interfaces_opt:
 | /*(*empty*)*/  { [] }
 | extends_interfaces  { $1 }


interface_member_declarations:
 | interface_member_declaration  { $1 }
 | interface_member_declarations interface_member_declaration  { $1 @ $2 }

interface_member_declarations_opt:
 | /*(*empty*)*/  { [] }
 | interface_member_declarations  { $1 }


variable_initializers:
 | variable_initializer  { [$1] }
 | variable_initializers CM variable_initializer  { $3 :: $1 }

comma_opt:
 | /*(*empty*)*/  { () }
 | CM  { () }


block_statements:
 | block_statement  { $1 }
 | block_statements block_statement  { $1 @ $2 }

block_statements_opt:
 | /*(*empty*)*/      { [] }
 | block_statements  { $1 }


switch_block_statement_groups:
 | switch_block_statement_group  { [$1] }
 | switch_block_statement_groups switch_block_statement_group  { $2 :: $1 }


switch_labels:
 | switch_label  { [$1] }
 | switch_labels switch_label  { $2 :: $1 }



expression_opt:
 | /*(*empty*)*/  { None }
 | expression     { Some $1 }


for_update_opt:
 | /*(*empty*)*/  { [] }
 | for_update     { $1 }

statement_expression_list:
 | statement_expression                               { [$1] }
 | statement_expression_list CM statement_expression  { $1 @ [$3] }

identifier_opt:
 | /*(*empty*)*/  { None }
 | identifier  { Some $1 }


catches:
 | catch_clause  { [$1] }
 | catches catch_clause  { $2 :: $1 }

catches_opt:
 | /*(*empty*)*/  { [] }
 | catches  { List.rev $1 }


argument_list:
 | expression  { [$1] }
 | argument_list CM expression  { $3 :: $1 }

argument_list_opt:
 | /*(*empty*)*/  { [] }
 | argument_list  { List.rev $1 }

method_declarations_opt:
 | /*(*empty*)*/  { [] }
 | method_declarations  { List.rev $1 }

method_declarations:
 | method_declaration { [$1] }
 | method_declarations method_declaration { $2 :: $1 }

class_body_opt:
 | /*(*empty*)*/  { None }
 | class_body     { Some $1 }

dim_exprs:
 | dim_expr  { [$1] }
 | dim_exprs dim_expr  { $2 :: $1 }

dims_opt:
 | /*(*empty*)*/  { 0 }
 | dims  { $1 }


enum_constants:
 | enum_constant { [$1] }
 | enum_constants CM enum_constant { $1 @ [$3] }

enum_body_declarations_opt:
 | /*(*empty*)*/           { [] }
 | enum_body_declarations  { $1 }

type_parameters_opt:
 | /*(*empty*)*/   { [] }
 | type_parameters { $1 }

type_parameters:
 | LT type_parameters_bis GT { $2 }

type_parameters_bis:
 | type_parameter                         { [$1] }
 | type_parameters_bis CM type_parameter  { $1 @ [$3] }

type_arguments:
 | type_argument                    { [$1] }
 | type_arguments CM type_argument  { $1 @ [$3] }

element_value_pairs:
 | element_value_pair { [$1] }
 | element_value_pairs CM element_value_pair { $1 @ [$3] }

element_values:
 | element_value { [$1] }
 | element_values CM element_value { $1 @ [$3] }
