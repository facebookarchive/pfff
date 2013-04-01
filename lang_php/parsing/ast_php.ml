(*s: ast_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2009-2011 Facebook
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
 *)
(*e: Facebook copyright *)
open Common

open Parse_info
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module defines an Abstract Syntax Tree for PHP 5.2 with
 * a few PHP 5.3 (e.g. closures) and 5.4 (e.g. traits) extensions as well
 * as support for a few Facebook extensions (XHP, generators, annotations).
 *
 * This is actually more a concrete syntax tree (CST) than an AST. This
 * is convenient in a refactoring context or code visualization
 * context, but if you need to do some heavy static analysis, consider
 * instead lang_php/analyze/foundation/pil.ml which defines a
 * PHP Intermediate Language a la CIL.
 *
 * todo: maybe even in a refactoring context a PIL+comment
 * (see pretty/ast_pp.ml) would make more sense.
 *
 * NOTE: data from this type are often marshalled in berkeley DB tables
 * which means that if you add a new constructor or field in the types below,
 * you must erase the berkeley DB databases otherwise pfff
 * will probably finish with a segfault (OCaml serialization is not
 * type-safe). A hacky solution is to add new constructors only at the end
 * of a type definition.
 *
 * COUPLING: some programs in other languages (e.g. Python) may
 * use some of the pfff binding, or JSON/sexp exporters, so if you
 * change the name of constructors in this file, don't forget
 * to regenerate the JSON/sexp exporters, but also to modify the
 * dependent programs !!!! An easier solution is to not change this
 * file, or to only add new constructors.
 *
 * todo:
 *  - unify expr and lvalue
 *  - add fbstrict types in AST, not just in grammar
 *  - support for '...' fbstrict extension in parameters
 *  - less: add namespace in AST (also add in grammar)
 *
 *  - introduce QualifierDynamic and factorize things in lvalue type
 *  - unify toplevel statement vs statements? hmmm maybe not
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
(*s: AST info *)
(* Contains among other things the position of the token through
 * the Common.parse_info embedded inside it, as well as the
 * the transformation field that makes possible spatch.
 *)
type tok = Parse_info.info
and info = tok
(*x: AST info *)
(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * tok
(*x: AST info *)
and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok
and 'a angle = tok * 'a * tok
and 'a single_angle = tok * 'a * tok
and 'a comma_list = ('a, tok (* the comma *)) Common.either list
and 'a comma_list_dots =
  ('a, tok (* ... in parameters *), tok (* the comma *)) Common.either3 list
(*x: AST info *)
 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: AST info *)
(* ------------------------------------------------------------------------- *)
(* Name. See also analyze_php/namespace_php.ml  *)
(* ------------------------------------------------------------------------- *)
(*s: AST name *)
 (*s: type name *)
 (* Was called T_STRING in Zend, which are really just LABEL, see the lexer.
  * Why not factorize Name and XhpName together? Because I was not
  * sure originally some analysis should also be applied on Xhp
  * classes. Moreover there is two syntax for xhp: :x:base for 'defs'
  * and <x:base for 'uses', so having this xhp_tag allow us to easily do
  * comparison between xhp names.
  *)
 type name =
    | Name of string wrap
    (*s: type name hook *)
    (* xhp: for :x:foo the list is ["x";"foo"] *)
    | XhpName of xhp_tag wrap
    (*e: type name hook *)
 (*e: type name *)
 (* for :x:foo the list is ["x";"foo"] *)
 and xhp_tag = string list

 (*s: type dname *)
 (* D for dollar. Was called T_VARIABLE in the original PHP parser/lexer.
  * The string does not contain the '$'. The info itself will usually
  * contain it, but not always! Indeed if the variable we build comes
  * from an encapsulated strings as in  echo "${x[foo]}" then the 'x'
  * will be parsed as a T_STRING_VARNAME, and eventually lead to a DName,
  * even if in the text it appears as a name.
  * So this token is kind of a FakeTok sometimes.
  *
  * So if at some point you want to do some program transformation,
  * you may have to normalize this string wrap before moving it
  * in another context !!!
  *)
 and dname =
   | DName of string wrap
 (*e: type dname *)

 (*s: qualifiers *)
 and qualifier = class_name_or_kwd * tok (* :: *)
  and class_name_or_kwd =
   | ClassName of fully_qualified_class_name * (type_args option)
   (* Could also transform at parsing time all occurences of self:: and
    * parent:: by their respective names. But I prefer to have all the
    * PHP features somehow explicitely represented in the AST.
    *)
   | Self   of tok
   | Parent of tok
   (* php 5.3 late static binding (no idea why it's useful ...) *)
   | LateStatic of tok
   (* todo? Put ClassVar of dname here so can factorize some of the
    * StaticDynamicCall stuff in lvalue?
    *)
 and fully_qualified_class_name = name
 and type_args = hint_type comma_list single_angle
 (*e: qualifiers *)
(*e: AST name *)
(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(*s: AST type *)
(* This is used in Cast. For type analysis see type_php.ml *)
and ptype =
  | BoolTy
  | IntTy
  | DoubleTy (* float *)

  | StringTy

  | ArrayTy
  | ObjectTy
(*e: AST type *)
(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(*s: AST expression *)
(* I used to have a 'type expr = exprbis * exp_type_info' but it complicates
 * many patterns when working on expressions, and it turns out I never
 * implemented the type annotater. It's easier to do such annotater on
 * a real AST like the PIL. So just have this file be a simple concrete
 * syntax tree and no more.
 *)
and expr =
  (*s: type exp_info *)
  (*e: type exp_info *)
  | Lv of lvalue

  (* start of expr_without_variable in original PHP lexer/parser terminology *)
  | Sc of scalar

  (*s: exprbis other constructors *)
  | Binary  of expr * binaryOp wrap * expr
  | Unary   of unaryOp wrap * expr
  (*x: exprbis other constructors *)
  (* should be a statement ... *)
  | Assign    of lvalue * tok (* = *) * expr
  | AssignOp  of lvalue * assignOp wrap * expr
  | Postfix of rw_variable   * fixOp wrap
  | Infix   of fixOp wrap    * rw_variable
  (*x: exprbis other constructors *)
  (* PHP 5.3 allow 'expr ?: expr' hence the 'option' type below
   * from www.php.net/manual/en/language.operators.comparison.php#language.operators.comparison.ternary:
   * "Since PHP 5.3, it is possible to leave out the middle part of the
   * ternary operator. Expression
   * expr1 ?: expr3 returns expr1 if expr1 evaluates to TRUE, and expr3
   * otherwise."
   *)
  | CondExpr of expr * tok (* ? *) * expr option * tok (* : *) * expr
  (*x: exprbis other constructors *)
  | AssignList  of tok (* list *)  * list_assign comma_list paren *
        tok (* = *) * expr
  | ArrayLong of tok (* array *) * array_pair  comma_list paren
  (* php 5.4: https://wiki.php.net/rfc/shortsyntaxforarrays *)
  | ArrayShort of array_pair comma_list bracket
  | VectorLit of tok (* Vector *) * vector_elt comma_list brace
  | MapLit of tok (* Map/StableMap *) * map_elt comma_list brace
  (*x: exprbis other constructors *)
  | New of tok * class_name_reference * argument comma_list paren option
  | Clone of tok * expr
  (*x: exprbis other constructors *)
  | AssignRef of lvalue * tok (* = *) * tok (* & *) * lvalue
  | AssignNew of lvalue * tok (* = *) * tok (* & *) * tok (* new *) *
        class_name_reference *
        argument comma_list paren option
  (*x: exprbis other constructors *)
  | Cast of castOp wrap * expr
  | CastUnset of tok * expr (* ??? *)
  (*x: exprbis other constructors *)
  | InstanceOf of expr * tok * class_name_reference
  (*x: exprbis other constructors *)
  (* !The evil eval! *)
  | Eval of tok * expr paren
  (*x: exprbis other constructors *)
  (* Woohoo, viva PHP 5.3 *)
  | Lambda of lambda_def
  (*x: exprbis other constructors *)
  (* should be a statement ... *)
  | Exit of tok * (expr option paren) option
  | At of tok (* @ *) * expr
  | Print of tok * expr
  (*x: exprbis other constructors *)
  | BackQuote of tok * encaps list * tok
  (*x: exprbis other constructors *)
  (* should be at toplevel *)
  | Include     of tok * expr | IncludeOnce of tok * expr
  | Require     of tok * expr | RequireOnce of tok * expr
  (*x: exprbis other constructors *)
  | Empty of tok * lvalue paren
  | Isset of tok * lvalue comma_list paren
  (*e: exprbis other constructors *)

  (* xhp: *)
  | XhpHtml of xhp_html
  (* php-facebook-ext:
   *
   * todo: this should be at the statement level as there are only a few
   * forms of yield that hphp support (e.g. yield <expr>; and
   * <lval> = yield <expr>). One could then have a YieldReturn and YieldAssign
   * but this may change and none of the analysis in pfff need to
   * understand yield so for now just make it simple and add yield
   * at the expression level.
   *)
  | Yield of tok * expr
  | YieldBreak of tok * tok

  (*s: type exprbis hook *)
  (* only appear when process sgrep patterns *)
  | SgrepExprDots of tok
  (*x: type exprbis hook *)
  (* unparser: *)
  | ParenExpr of expr paren
  (*e: type exprbis hook *)

  (*s: type scalar and constant and encaps *)
    and scalar =
      | C of constant
      | ClassConstant of qualifier * name

      | Guil    of tok (* '"' or b'"' *) * encaps list * tok (* '"' *)
      | HereDoc of
          tok (* < < < EOF, or b < < < EOF *) *
          encaps list *
          tok  (* EOF; *)
      (* | StringVarName??? *)

   (*s: type constant *)
       and constant =
       (*s: constant constructors *)
        | Int of string wrap
        | Double of string wrap
       (*x: constant constructors *)
        (* see also Guil for interpolated strings
         * The string does not contain the enclosing '"' or "'".
         * It does not contain either the possible 'b' prefix
         *)
        | String of string wrap
       (*x: constant constructors *)
        | CName of name (* true, false, null,  or defined constant *)
       (*x: constant constructors *)
        | PreProcess of cpp_directive wrap
       (*e: constant constructors *)
       (*s: type constant hook *)
        (* only appear when process xdebug coverage file *)
        | XdebugClass of name * class_stmt list
        | XdebugResource
       (*e: type constant hook *)
       (*s: constant rest *)
        (*s: type cpp_directive *)
        (* http://php.net/manual/en/language.constants.predefined.php *)
          and cpp_directive =
              | Line  | File | Dir
              | ClassC | TraitC
              | MethodC  | FunctionC
        (*e: type cpp_directive *)
       (*e: constant rest *)
   (*e: type constant *)
   (*s: type encaps *)
       and encaps =
         (*s: encaps constructors *)
          | EncapsString of string wrap
         (*x: encaps constructors *)
          | EncapsVar of lvalue
         (*x: encaps constructors *)
          (* for "xx {$beer}s" *)
          | EncapsCurly of tok * lvalue * tok
         (*x: encaps constructors *)
          (* for "xx ${beer}s" *)
          | EncapsDollarCurly of tok (* '${' *) * lvalue * tok
         (*x: encaps constructors *)
          | EncapsExpr of tok * expr * tok
         (*e: encaps constructors *)
   (*e: type encaps *)
  (*e: type scalar and constant and encaps *)

  (*s: AST expression operators *)
   and fixOp    = Dec | Inc
   and binaryOp    = Arith of arithOp | Logical of logicalOp
     (*s: php concat operator *)
      | BinaryConcat (* . *)
     (*e: php concat operator *)
         and arithOp   =
           | Plus | Minus | Mul | Div | Mod
           | DecLeft | DecRight
           | And | Or | Xor

         and logicalOp =
           | Inf | Sup | InfEq | SupEq
           | Eq | NotEq
           (*s: php identity operators *)
           | Identical (* === *) | NotIdentical (* !== *)
           (*e: php identity operators *)
           | AndLog | OrLog | XorLog
           | AndBool | OrBool (* diff with AndLog ? short-circuit operators ? *)
   and assignOp = AssignOpArith of arithOp
    (*s: php assign concat operator *)
     | AssignConcat (* .= *)
    (*e: php assign concat operator *)
   and unaryOp =
     | UnPlus | UnMinus
     | UnBang | UnTilde

  (*x: AST expression operators *)
   and castOp = ptype
  (*e: AST expression operators *)

  (*s: AST expression rest *)
   and list_assign =
     | ListVar of lvalue
     | ListList of tok * list_assign comma_list paren
     | ListEmpty
  (*x: AST expression rest *)
   and array_pair =
     | ArrayExpr of expr
     | ArrayRef of tok (* & *) * lvalue
     | ArrayArrowExpr of expr * tok (* => *) * expr
     | ArrayArrowRef of expr * tok (* => *) * tok (* & *) * lvalue
   and vector_elt =
     | VectorExpr of expr
     | VectorRef of tok (* & *) * lvalue
   and map_elt =
     | MapArrowExpr of expr * tok (* => *) * expr
     | MapArrowRef of expr * tok (* => *) * tok (* & *) * lvalue
  (*x: AST expression rest *)
   and class_name_reference =
     | ClassNameRefStatic of class_name_or_kwd
     | ClassNameRefDynamic of lvalue * obj_prop_access list

     and obj_prop_access = tok (* -> *) * obj_property
  (*e: AST expression rest *)

 and xhp_html =
   | Xhp of xhp_tag wrap * xhp_attribute list * tok (* > *) *
       xhp_body list * xhp_tag option wrap
   | XhpSingleton of xhp_tag wrap * xhp_attribute list * tok (* /> *)

   and xhp_attribute = xhp_attr_name * tok (* = *) * xhp_attr_value
    and xhp_attr_name = string wrap (* e.g. task-bar *)
    and xhp_attr_value =
      | XhpAttrString of tok (* '"' *) * encaps list * tok (* '"' *)
      | XhpAttrExpr of expr brace
      (* sgrep: *)
      | SgrepXhpAttrValueMvar of string wrap
   and xhp_body =
     | XhpText of string wrap
     | XhpExpr of expr brace
     | XhpNested of xhp_html

(*e: AST expression *)
(* ------------------------------------------------------------------------- *)
(* Variable (which in fact also contains function calls) *)
(* ------------------------------------------------------------------------- *)
(*s: AST lvalue *)
and lvalue =
  (*s: type lvalue_info *)
  (*e: type lvalue_info *)
  (*s: lvaluebis constructors *)
    | Var of dname *
     (*s: scope_php annotation *)
     Scope_php.phpscope ref
     (*e: scope_php annotation *)
  (*x: lvaluebis constructors *)
    | This of tok
    (* for (new Foo)->bar(), ugly: when expr = lvalue, no need that *)
    | NewLv of 
        (tok * class_name_reference * argument comma_list paren option) paren
    (* xhp: normally we can not have a FunCall in the lvalue of VArrayAccess,
     * but with xhp we can.
     *
     * todo? a VArrayAccessSimple with Constant string in expr ?
     *)
    | VArrayAccess of lvalue * expr option bracket
    | VArrayAccessXhp of expr * expr option bracket
  (*x: lvaluebis constructors *)
    | VBrace       of tok    * expr brace
    | VBraceAccess of lvalue * expr brace
  (*x: lvaluebis constructors *)
    (* on the left of var *)
    | Indirect  of lvalue * indirect
  (*x: lvaluebis constructors *)
    (* Note that even if A::$v['fld'] was parsed in the grammar
     * as a Qualifier(A, ArrayAccess($v, 'fld') we
     * generate a ArrayAccess(Qualifier(A, $v), 'fld').
     * todo? could merge 3 cases if qualifier allow some dname.
     *)
    | VQualifier of qualifier * lvalue
    (* note that can be a late static class var since php 5.3 *)
    | ClassVar of qualifier * dname
    (* used to be lvalue * dname but can have code like $class::$$prop *)
    | DynamicClassVar of lvalue * tok (* :: *) * lvalue
  (*x: lvaluebis constructors *)
    | FunCallSimple of name                      * argument comma_list paren
    (* DynamicFunCall *)
    | FunCallVar    of qualifier option * lvalue * argument comma_list paren
  (*x: lvaluebis constructors *)
    (* note that can be a late static call since php 5.3 *)
    | StaticMethodCallSimple of qualifier * name * argument comma_list paren
    | MethodCallSimple of lvalue * tok * name    * argument comma_list paren
    (* PHP 5.3 *)
    | StaticMethodCallVar of lvalue * tok (* :: *) * name *
        argument comma_list paren
    | StaticObjCallVar of lvalue * tok (* :: *) * lvalue *
        argument comma_list paren
  (*x: lvaluebis constructors *)
    | ObjAccessSimple of lvalue * tok (* -> *) * name
    | ObjAccess of lvalue * obj_access
  (*e: lvaluebis constructors *)

  (*s: type lvalue aux *)
    and indirect = Dollar of tok
  (*x: type lvalue aux *)
    and argument =
      | Arg    of expr
      | ArgRef of tok * w_variable
  (*x: type lvalue aux *)
    and obj_access =
     tok (* -> *) * obj_property * argument comma_list paren option

    and obj_property =
      | ObjProp of obj_dim
      | ObjPropVar of lvalue (* was originally var_without_obj *)

      (* I would like to remove OName from here, as I inline most of them
       * in the MethodCallSimple and ObjAccessSimple above, but they
       * can also be mentionned in OArrayAccess in the obj_dim, so
       * I keep it
       *)
      and obj_dim =
        | OName of name
        | OBrace of expr brace
        | OArrayAccess of obj_dim * expr option bracket
        | OBraceAccess of obj_dim * expr brace
  (*e: type lvalue aux *)

(* semantic: those grammar rule names were used in the original PHP
 * lexer/parser but not enforced. It's just comments. *)
and rw_variable = lvalue
and r_variable = lvalue
and w_variable = lvalue

(*e: AST lvalue *)
(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
(*s: AST statement *)
(* By introducing Lambda, expr and stmt are now mutually recursive *)
and stmt =
  (*s: stmt constructors *)
    | ExprStmt of expr * tok (* ; *)
    | EmptyStmt of tok  (* ; *)
  (*x: stmt constructors *)
    | Block of stmt_and_def list brace
  (*x: stmt constructors *)
    | If      of tok * expr paren * stmt *
        (* elseif *) if_elseif list *
        (* else *)   if_else option
    (*s: ifcolon *)
    | IfColon of tok * expr paren *
          tok * stmt_and_def list * new_elseif list * new_else option *
          tok * tok
      (* if(cond):
       *   stmts; defs;
       * elseif(cond):
       *   stmts;..
       * else(cond):
       *   defs; stmst;
       * endif; *)
    (*e: ifcolon *)
    | While of tok * expr paren * colon_stmt
    | Do of tok * stmt * tok * expr paren * tok
    | For of tok * tok *
        for_expr * tok *
        for_expr * tok *
        for_expr *
        tok *
        colon_stmt
    | Switch of tok * expr paren * switch_case_list
  (*x: stmt constructors *)
    (* if it's a expr_without_variable, the second arg must be a Right variable,
     * otherwise if it's a variable then it must be a foreach_variable
     *)
    | Foreach of tok * tok * expr * tok * foreach_var_either *
        foreach_arrow option * tok *
        colon_stmt
      (* example: foreach(expr as $lvalue) { colon_stmt }
       *          foreach(expr as $foreach_varialbe => $lvalue) { colon_stmt}
       *)
  (*x: stmt constructors *)
    | Break    of tok * expr option * tok
    | Continue of tok * expr option * tok
    | Return of tok * expr option * tok
  (*x: stmt constructors *)
    | Throw of tok * expr * tok
    | Try of tok * stmt_and_def list brace * catch * catch list
  (*x: stmt constructors *)
    | Echo of tok * expr comma_list * tok
  (*x: stmt constructors *)
    | Globals    of tok * global_var comma_list * tok
    | StaticVars of tok * static_var comma_list * tok
  (*x: stmt constructors *)
    | InlineHtml of string wrap
  (*x: stmt constructors *)
    | Use of tok * use_filename * tok
    | Unset of tok * lvalue comma_list paren * tok
    | Declare of tok * declare comma_list paren * colon_stmt
  (*e: stmt constructors *)
    (* static-php-ext: *)
    | TypedDeclaration of hint_type * lvalue * (tok * expr) option * tok

    (* was in stmt_and_def before *)
    | FuncDefNested of func_def
    | ClassDefNested of class_def

  (*s: AST statement rest *)
    and switch_case_list =
      | CaseList      of
          tok (* { *) * tok option (* ; *) * case list * tok (* } *)
      | CaseColonList of
          tok (* : *) * tok option (* ; *) * case list *
          tok (* endswitch *) * tok (* ; *)
      and case =
        | Case    of tok * expr * tok * stmt_and_def list
        | Default of tok * tok * stmt_and_def list

   and if_elseif = tok * expr paren * stmt
   and if_else = (tok * stmt)
  (*x: AST statement rest *)
    and for_expr = expr comma_list (* can be empty *)
    and foreach_arrow = tok * foreach_variable
    and foreach_variable = is_ref * lvalue
    and foreach_var_either = (foreach_variable, lvalue) Common.either
  (*x: AST statement rest *)
    and catch =
      tok * (fully_qualified_class_name * dname) paren * stmt_and_def list brace
  (*x: AST statement rest *)
    and use_filename =
      | UseDirect of string wrap
      | UseParen  of string wrap paren
  (*x: AST statement rest *)
    and declare = name * static_scalar_affect
  (*x: AST statement rest *)
    and colon_stmt =
      | SingleStmt of stmt
      | ColonStmt of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *)
  (*x: AST statement rest *)
    and new_elseif = tok * expr paren * tok * stmt_and_def list
    and new_else = tok * tok * stmt_and_def list
  (*e: AST statement rest *)
(*e: AST statement *)
(* ------------------------------------------------------------------------- *)
(* Function (and method) definition *)
(* ------------------------------------------------------------------------- *)
(*s: AST function definition *)
and func_def = {
  f_attrs: attributes option;
  f_tok: tok; (* function *)
  f_type: function_type;
  (* only valid for methods *)
  f_modifiers: modifier wrap list;
  f_ref: is_ref;
  (* can be a Name("__lambda", f_tok) when used for lambdas *)
  f_name: name;
  f_params: parameter comma_list_dots paren; (* TODO: handle ... *)
  (* static-php-ext: *)
  f_return_type: hint_type option;
  (* the opening/closing brace can be (fakeInfo(), ';') for abstract methods *)
  f_body: stmt_and_def list brace;
  (*s: f_type mutable field *)
  (*e: f_type mutable field *)
}
    and function_type =
      | FunctionRegular
      | FunctionLambda
      | MethodRegular
      | MethodAbstract

  (*s: AST function definition rest *)
    and parameter = {
      p_attrs: attributes option;
      p_type: hint_type option;
      p_ref: is_ref;
      p_name: dname;
      p_default: static_scalar_affect option;
    }
  (*x: AST function definition rest *)
      and hint_type =
        | Hint of class_name_or_kwd (* only self/parent, no static *)
        | HintArray of tok
        | HintQuestion of (tok * hint_type)
        | HintTuple of hint_type comma_list paren
        | HintCallback of
            (tok                                 (* "function" *)
             * (hint_type comma_list_dots paren) (* params *)
             * hint_type option)                 (* return type *)
            paren
  (*x: AST function definition rest *)
    and is_ref = tok (* bool wrap ? *) option
  (*e: AST function definition rest *)
(*e: AST function definition *)
(*s: AST lambda definition *)
(* the f_name in func_def should be a fake name *)
and lambda_def = (lexical_vars option * func_def)
  and lexical_vars = tok (* use *) * lexical_var comma_list paren
  and lexical_var = LexicalVar of is_ref * dname

(* ------------------------------------------------------------------------- *)
(* Constant definition *)
(* ------------------------------------------------------------------------- *)
(* todo use record *)
and constant_def = tok * name * tok (* = *) * static_scalar * tok (* ; *)

(*e: AST lambda definition *)
(* ------------------------------------------------------------------------- *)
(* Class (and interface/trait) definition *)
(* ------------------------------------------------------------------------- *)
(*s: AST class definition *)
(* I used to have a class_def and interface_def because interface_def
 * didn't allow certain forms of statements (methods with a body), but
 * with the introduction of traits, it does not make that much sense
 * to be so specific, so I factorized things. Classes/interfaces/traits
 * are not that different. Interfaces are really just abstract traits.
 *)
and class_def = {
  c_attrs: attributes option;
  c_type: class_type;
  c_name: name;
  (* PHP uses single inheritance. Interfaces can also use 'extends'
   * but we use the c_implements field for that (because it can be a list).
   *)
  c_extends: extend option;
  (* For classes it's a list of interfaces, for interface a list of other
   * interfaces it extends, and for traits it must be empty.
   *)
  c_implements: interface option;
  (* The class_stmt for interfaces are restricted to only abstract methods.
   * The class_stmt seems to be unrestricted for traits; can even
   * have some 'use' *)
  c_body: class_stmt list brace;
}
  (*s: type class_type *)
    and class_type =
      | ClassRegular  of tok (* class *)
      | ClassFinal    of tok * tok (* final class *)
      | ClassAbstract of tok * tok (* abstract class *)

      | Interface of tok (* interface *)
      (* PHP 5.4 traits: http://php.net/manual/en/language.oop5.traits.php
       * Allow to mixin behaviors and data so it's really just
       * multiple inheritance with a cooler name.
       *
       * note: traits are allowed only at toplevel.
       *)
      | Trait of tok (* trait *)
  (*e: type class_type *)
  (*s: type extend *)
    and extend =    tok * fully_qualified_class_name
  (*e: type extend *)
  (*s: type interface *)
    and interface = tok * fully_qualified_class_name comma_list
  (*e: type interface *)
(*x: AST class definition *)
(*x: AST class definition *)
  and class_stmt =
    | ClassConstants of tok (* const *) * class_constant comma_list * tok (*;*)
    | ClassVariables of
        class_var_modifier *
         (* static-php-ext: *)
          hint_type option *
        class_variable comma_list * tok (* ; *)
    | Method of method_def

    | XhpDecl of xhp_decl
    (* php 5.4, 'use' can appear in classes/traits (but not interface) *)
    | UseTrait of tok (*use*) * name comma_list *
        (tok (* ; *), trait_rule list brace) Common.either

    (*s: class_stmt types *)
        and class_constant = name * static_scalar_affect
    (*x: class_stmt types *)
        and class_variable = dname * static_scalar_affect option
    (*x: class_stmt types *)
        and class_var_modifier =
          | NoModifiers of tok (* 'var' *)
          | VModifiers of modifier wrap list
    (*x: class_stmt types *)
        (* a few special names: __construct, __call, __callStatic
         * ugly: f_body is an empty stmt_and_def for abstract method
         * and the ';' is put for the info of the closing brace
         * (and the opening brace is a fakeInfo).
         *)
        and method_def = func_def
    (*x: class_stmt types *)
          and modifier =
            | Public  | Private | Protected
            | Static  | Abstract | Final
    (*x: class_stmt types *)
    (*e: class_stmt types *)
 and xhp_decl =
    | XhpAttributesDecl of
        tok (* attribute *) * xhp_attribute_decl comma_list * tok (*;*)
    (* there is normally only one 'children' declaration in a class *)
    | XhpChildrenDecl of
        tok (* children *) * xhp_children_decl * tok (*;*)
    | XhpCategoriesDecl of
        tok (* category *) * xhp_category_decl comma_list * tok (*;*)

 and xhp_attribute_decl =
   | XhpAttrInherit of xhp_tag wrap
   | XhpAttrDecl of xhp_attribute_type * xhp_attr_name *
       xhp_value_affect option * tok option (* is required *)
   and xhp_attribute_type =
     | XhpAttrType of hint_type
     | XhpAttrEnum of tok (* enum *) * constant comma_list brace
  and xhp_value_affect = tok (* = *) * static_scalar

 (* Regexp-like syntax. The grammar actually restricts what kinds of
  * regexps can be written. For instance pcdata must be nested. But
  * here I simplified the type.
  *)
 and xhp_children_decl =
   | XhpChild of xhp_tag wrap (* :x:frag *)
   | XhpChildCategory of xhp_tag wrap (* %x:frag *)

   | XhpChildAny of tok
   | XhpChildEmpty of tok
   | XhpChildPcdata of tok

   | XhpChildSequence    of xhp_children_decl * tok (*,*) * xhp_children_decl
   | XhpChildAlternative of xhp_children_decl * tok (*|*) * xhp_children_decl

   | XhpChildMul    of xhp_children_decl * tok (* * *)
   | XhpChildOption of xhp_children_decl * tok (* ? *)
   | XhpChildPlus   of xhp_children_decl * tok (* + *)

   | XhpChildParen of xhp_children_decl paren

 and xhp_category_decl = xhp_tag wrap (* %x:frag *)

(* todo: 'as' and 'insteadof', but those are bad features ... noone should
 * use them.
 *)
and trait_rule = unit

(*e: AST class definition *)
(* ------------------------------------------------------------------------- *)
(* Other declarations *)
(* ------------------------------------------------------------------------- *)
(*s: AST other declaration *)
and global_var =
  | GlobalVar of dname
  | GlobalDollar of tok * r_variable
  | GlobalDollarExpr of tok * expr brace
(*x: AST other declaration *)
and static_var = dname * static_scalar_affect option
(*x: AST other declaration *)
  (* static_scalar used to be a special type allowing constants and
   * a restricted form of expressions. But it was yet
   * another type and it turned out it was making things like spatch
   * and visitors more complicated because stuff like "+ 1" could
   * be an expr or a static_scalar. We don't need this "isomorphism".
   * I never leveraged the specificities of static_scalar (maybe a compiler
   * would, but my checker/refactorers/... don't).
   *
   * Note that it's not 'type static_scalar = scalar' because static_scalar
   * actually allows arrays (why the heck they called it a scalar then ....)
   * and plus/minus which are only in expr.
   *)
  and static_scalar = expr
  (*s: type static_scalar hook *)
  (*e: type static_scalar hook *)
(*x: AST other declaration *)
   and static_scalar_affect = tok (* = *) * static_scalar
(*x: AST other declaration *)
(*e: AST other declaration *)
(*s: AST statement bis *)
(* stmt_and_def used to be a special type allowing Stmt or nested functions
 * or classes but it was introducing yet another, not so useful, intermediate
 * type.
 *)
and stmt_and_def = stmt
(*e: AST statement bis *)
(* ------------------------------------------------------------------------- *)
(* phpext: *)
(* ------------------------------------------------------------------------- *)
(*s: AST phpext *)
(* HPHP extension similar to http://en.wikipedia.org/wiki/Java_annotation *)
and attribute =
  | Attribute of string wrap
  | AttributeWithArgs of string wrap * static_scalar comma_list paren

and attributes = attribute comma_list angle

(*e: AST phpext *)
(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
(* For parsing reasons and estet I think it's better to differentiate
 * nested function and toplevel functions. Also it's better to
 * group the toplevel statements together (StmtList below), so that
 * in the database later they share the same id.
 *
 * Note that nested functions are usually under a if(defined(...)) at
 * the toplevel. There is no ifdef in PHP so they reuse if.
 *)
(*s: AST toplevel *)
and toplevel =
  (*s: toplevel constructors *)
    | StmtList of stmt list
    | FuncDef of func_def
    | ClassDef of class_def
    (* PHP 5.3, see http://us.php.net/const *)
    | ConstantDef of constant_def
    (* old:  | Halt of tok * unit paren * tok (* __halt__ ; *) *)
  (*x: toplevel constructors *)
    | NotParsedCorrectly of tok list (* when Flag.error_recovery = true *)
  (*x: toplevel constructors *)
    | FinalDef of tok (* EOF *)
  (*e: toplevel constructors *)
 and program = toplevel list
 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)

(*e: AST toplevel *)
(* ------------------------------------------------------------------------- *)
(* Entity and any *)
(* ------------------------------------------------------------------------- *)
(*s: AST entity *)
(* The goal of the entity type is to lift up important entities which
 * are originally nested in the AST such as methods.
 *
 * history: was in ast_entity_php.ml before but better to put everything
 * in one file.
 *)
type entity =
  | FunctionE of func_def
  | ClassE of class_def
  | ConstantE of constant_def

  | StmtListE of stmt list

  | MethodE of method_def

  | ClassConstantE of class_constant
  | ClassVariableE of class_variable * modifier list
  | XhpAttrE of xhp_attribute_decl

  | MiscE of tok list
(*e: AST entity *)
(*s: AST any *)
type any =
  | Expr of expr
  | Stmt2 of stmt
  | Lvalue of lvalue
  | StmtAndDefs of stmt_and_def list
  | Toplevel of toplevel
  | Program of program
  | Entity of entity

  | Argument of argument
  | Arguments of argument comma_list
  | Parameter of parameter
  | Parameters of parameter comma_list_dots paren
  | Body of stmt_and_def list brace

  | ClassStmt of class_stmt
  | ClassConstant2 of class_constant
  | ClassVariable of class_variable
  | ListAssign of list_assign
  | ColonStmt2 of colon_stmt
  | Case2 of case

  | XhpAttribute of xhp_attribute
  | XhpAttrValue of xhp_attr_value
  | XhpHtml2 of xhp_html
  | XhpChildrenDecl2 of xhp_children_decl

  | Info of tok
  | InfoList of tok list

  | Name2 of name
  | ClassNameRef of class_name_reference
  | Hint2 of hint_type

 (* with tarzan *)
(*e: AST any *)

(*****************************************************************************)
(* Comments *)
(*****************************************************************************)
(*x: ast_php.ml *)
(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
let noScope () = ref (Scope_code.NoScope)

let fakeInfo ?(next_to=None) str = {
  token = FakeTokStr (str, next_to);
  comments = ();
  transfo = NoTransfo;
  }
(*x: ast_php.ml *)
(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let unwrap = fst

let unparen (a,b,c) = b
let unbrace = unparen
let unbracket = unparen

let uncomma xs = Common.map_filter (function
  | Left e -> Some e
  | Right info -> None
  ) xs

let uncomma_dots xs = Common.map_filter (function
  | Left3 e -> Some e
  | Right3 info | Middle3 info -> None
  ) xs

let map_paren f (lp, x, rp) = (lp, f x, rp)
let map_comma_list f xs = List.map (fun x ->
  match x with
  | Left e -> Left (f e)
  | Right tok -> Right tok
  )
  xs

let unarg arg =
  match arg with
  | Arg e -> e
  | ArgRef _ -> failwith "Found a ArgRef"

let unargs xs =
  uncomma xs +> Common.partition_either (function
  | Arg e -> Left e
  | ArgRef (t, e) -> Right (e)
  )

let unmodifiers class_vars =
  match class_vars with
  | NoModifiers _ -> []
  | VModifiers xs -> List.map unwrap xs

(*x: ast_php.ml *)
(*x: ast_php.ml *)
(*x: ast_php.ml *)
let str_of_info x = Parse_info.str_of_info x
let col_of_info x = Parse_info.col_of_info x
let line_of_info x = Parse_info.line_of_info x
(* todo: return a Real | Virt position ? *)
let pos_of_info x = Parse_info.pos_of_info x
let file_of_info x = Parse_info.file_of_info x
(*x: ast_php.ml *)
let pinfo_of_info = Parse_info.pinfo_of_info
(*x: ast_php.ml *)
let rewrap_str = Parse_info.rewrap_str
(*x: ast_php.ml *)
(* for error reporting *)
let string_of_info x = Parse_info.string_of_info x
let is_origintok = Parse_info.is_origintok

type posrv = Parse_info.posrv

let compare_pos ii1 ii2 =
  let get_pos = function
    | OriginTok pi -> Real pi
    | FakeTokStr (s, Some (pi_orig, offset)) ->
        Virt (pi_orig, offset)
    | FakeTokStr _
    | Ab
      -> failwith "get_pos: Ab or FakeTok"
    | ExpandedTok (pi_pp, pi_orig, offset) ->
        Virt (pi_orig, offset)
  in
  let pos1 = get_pos (pinfo_of_info ii1) in
  let pos2 = get_pos (pinfo_of_info ii2) in
  match (pos1,pos2) with
  | (Real p1, Real p2) ->
      compare p1.Parse_info.charpos p2.Parse_info.charpos
  | (Virt (p1,_), Real p2) ->
      if (compare p1.Parse_info.charpos p2.Parse_info.charpos) =|= (-1)
      then (-1)
      else 1
  | (Real p1, Virt (p2,_)) ->
      if (compare p1.Parse_info.charpos p2.Parse_info.charpos) =|= 1
      then 1
      else (-1)
  | (Virt (p1,o1), Virt (p2,o2)) ->
      let poi1 = p1.Parse_info.charpos in
      let poi2 = p2.Parse_info.charpos in
      match compare poi1 poi2 with
      |	-1 -> -1
      |	0 -> compare o1 o2
      |	1 -> 1
      | _ -> raise Impossible
(*x: ast_php.ml *)
(*x: ast_php.ml *)
(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the AST to add some info about the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' to compare Ast elements. To overcome this problem, to be
 * able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" (al) information.
 *)

let al_info x =
  { x with token = Ab }
(*x: ast_php.ml *)
(*****************************************************************************)
(* Views *)
(*****************************************************************************)

(* examples:
 * inline more static funcall in expr type or variable type
 *
 *)
(*x: ast_php.ml *)
(*****************************************************************************)
(* Helpers, could also be put in lib_parsing.ml instead *)
(*****************************************************************************)
let name e =
  match e with
  | Name x -> unwrap x
  | XhpName (xs, _tok) ->
      ":" ^ (Common.join ":" xs)
let str_of_name x = name x

let dname (DName x) = unwrap x
let str_of_dname x = dname x

(*x: ast_php.ml *)
let info_of_name e =
  match e with
  | (Name (x,y)) -> y
  | (XhpName (x,y)) -> y
let info_of_dname (DName (x,y)) = y
(*e: ast_php.ml *)
