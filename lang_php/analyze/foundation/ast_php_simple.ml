(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* A (real) Abstract Syntax Tree for PHP, not a Concrete Syntax Tree
 * as in ast_php.ml
 *
 * This file contains a simplified PHP abstract syntax tree. The original
 * PHP syntax tree (ast_php.ml) is good for code refactoring or
 * code visualization; the type used is very precise, However, for
 * other algorithms, the nature of the AST makes the code a bit
 * redundant. Say I want to write a typechecker, I need to write a
 * specific version for static expressions, when really, the checker
 * should do the same thing. The same is true for a pretty-printer,
 * topological sort etc ... Hence the idea of a SimpleAST which is the
 * original AST where the specialised constructions have been factored
 * back together.
 *
 * Here is a partial list of the simplications/factorizations:
 *  - no tokens in the AST like parenthesis, brackets, etc. No ParenExpr.
 *    The only token information kept is for identifiers (see wrap below)
 *    for error reporting.
 *  - support for old syntax is removed such as IfColon
 *  - support for extra tools is removed such as Xdebug or Sgrep
 *  - sugar is removed, no ArrayLong vs ArrayShort, no InlineHtml,
 *    no HereDoc, no EncapsXxx
 *  - some builtins, for instance echo are transformed in "__builtin__echo".
 *    See builtin() and special() below
 *  - a simpler stmt type; no extra toplevel, stmt_and_def types
 *  - a simpler expr type; no lvalue vs expr vs static_scalar,
 *    no FunCallSimple vs FunCallVar, VarrayAccess vs VarrayAccessXhp,
 *  - unified class and object access via Class_get and Obj_get instead
 *    of lots of duplication in many constructors
 *  - a simpler name; identifiers, xhp names, variables are unified.
 *  - ...
 *
 * todo: factorize more? string vs Guil vs xhp?
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* to get position information for certain elements in the AST *)
type 'a wrap = 'a * Ast_php.tok

type program = stmt list

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and stmt =
  | Expr of expr

  | Block of stmt list

  | If of expr * stmt * stmt
  | Switch of expr * case list

  | While of expr * stmt list
  | Do of stmt list * expr
  | For of expr list * expr list * expr list * stmt list
  | Foreach of expr * expr * expr option * stmt list

  | Return of expr option
  | Break of expr option | Continue of expr option

  | Throw of expr
  | Try of stmt list * catch * catch list

  (* only at toplevel in most of our code *)
  | ClassDef of class_def
  | FuncDef of func_def
  (* only at toplevel *)
  | ConstantDef of constant_def

  (* pad: ?? *)
  | StaticVars of (string wrap * expr option) list
  | Global of expr list

  and case =
    | Case of expr * stmt list
    | Default of stmt list

  (* catch(Exception $exn) { ... } => ("Exception", "$exn", [...]) *)
  (* todo: use wrap *)
  and catch = string  * string * stmt list

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr =
  (* booleans are really just Int in PHP :( *)
  | Int of string
  | Double of string
  | String of string

  (* Id is valid for "entities" (functions, classes, constants) and variables.
   * So can have Id "foo" and Id "$foo". Can also contain "self/parent".
   * Can also be "true", "false", "null" and many other builtin constants.
   *
   * todo? Introduce a Var of string wrap? can be good to differentiate
   * them no? At the same time OCaml does not ...
   *)
  | Id of string wrap

  (* when None it means add to the end when used in lvalue position *)
  | Array_get of expr * expr option

  (* often transformed in Id "$this" in the analysis *)
  | This
  (* Unified method call access.
   * ex: $o->foo ==> Obj_get(Id "$o", Id "foo")
   * ex: A::foo  ==> Class_get(Id "A", Id "foo")
   * note that Id can be "self", "parent", "static".
   *)
  | Obj_get of expr * expr
  | Class_get of expr * expr

  | New of expr * expr list
  | InstanceOf of expr * expr

  (* pad: could perhaps be at the statement level? *)
  | Assign of Ast_php.binaryOp option * expr * expr
  (* really a destructuring tuple let always used in an Assign *)
  | List of expr list

  | Call of expr * expr list

  (* todo? transform into Call (builtin ...) ? *)
  | Infix of Ast_php.fixOp * expr
  | Postfix of Ast_php.fixOp * expr
  | Binop of Ast_php.binaryOp * expr * expr
  | Unop of Ast_php.unaryOp * expr
  | Guil of expr list

  | Ref of expr

  | ConsArray of array_value list
  | Xhp of xml

  | CondExpr of expr * expr * expr
  | Cast of Ast_php.ptype * expr

  (* yeah! PHP 5.3 is becoming a real language *)
  | Lambda of func_def

  and array_value =
    | Aval of expr
    | Akval of expr * expr

  (* pad: do we need that? could convert into something more basic *)
  and xhp =
    | XhpText of string
    | XhpExpr of expr
    | XhpXml of xml

    (* todo: use some string wrap *)
    and xml = {
      xml_tag: string list;
      xml_attrs: (string * xhp_attr) list;
      xml_body: xhp list;
    }
     and xhp_attr = expr

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

(* TODO: no 'uses' field for lambda? because we will use ocaml closures
 * for representing closures :) so during abstract interpretation for
 * instance the environment will be closed?
 *)
and func_def = {
  f_ref: bool;
  f_name: string wrap; (* _lambda when used for lambda *)
  f_params: parameter list;
  f_return_type: hint_type option;
  f_body: stmt list;
}

   and parameter = {
     p_type: hint_type option;
     p_ref: bool;
     p_name: string wrap;
     p_default: expr option;
   }

   and hint_type =
     | Hint of string
     | HintArray

and constant_def = {
  cst_name: string wrap;
  (* normally a static scalar *)
  cst_body: expr;
}

and class_def = {
  c_type: class_type;
  c_name: string wrap;
  c_extends: string list; (* pad: ?? *)
  c_traits: string wrap list;
  c_implements: string list;

  c_constants: (string * expr) list;
  c_variables: class_var list;
  c_methods: method_def list;
}

  and class_type =
    | ClassRegular
    | ClassFinal
    | ClassAbstract
    | Interface
    | Trait

  and class_var = {
    cv_name: string;
    cv_type: hint_type option;
    (* todo: could have a cv_name, cv_val and inline the list *)
    cv_value: expr option;
    cv_final: bool; cv_static: bool; cv_abstract: bool;
    cv_visibility: visibility;
  }

  and method_def = {
    m_name: string wrap;
    m_ref: bool;
    m_params: parameter list;
    m_return_type: hint_type option;
    m_body: stmt list;
    (* factorize with class_vars? *)
    m_static: bool; m_final: bool;m_abstract: bool;
    m_visibility: visibility;
  }

   and visibility =
     | Novis
     | Public  | Private
     | Protected | Abstract

 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unwrap x = fst x
let wrap s = s, Ast_php.fakeInfo s

(* for echo, eval, print, unset, isset, etc *)
let builtin x = "__builtin__" ^ x
(* for self, parent, static, lambdas *)
let special x = "__special__" ^ x

let has_modifier cv =
  cv.cv_final ||
  cv.cv_static ||
  cv.cv_abstract ||
  cv.cv_visibility <> Novis

let string_of_xhp_tag xs = Common.join ":" xs
