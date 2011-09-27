(*s: pil.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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

module Ast = Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * The Ast_php module is nice when you want to do some simple analysis
 * or refactoring as it directly matches the source code; error reporting
 * is precise and unparsing a modified AST leads to a file that is
 * close to the original coding style. Unfortunately this AST, which
 * is in fact more a CST for Concrete Syntax Tree is not very convenient
 * for heavier static analysis such as data flow tracking. 
 * 
 * Indeed, PHP is a large language and as a consequence Ast_php contains 
 * lots of constructors with many many cases to handle for the analysis 
 * writer. For instance incrementing a variable can be written as
 *  $i = $i + 1; or $i++; or ++$i;  which in our AST is represented by
 * 3 different constructs.  Moreover some important constructs
 * such as function calls or assignements can be very
 * deeply nested inside an expression. One can obviously write in PHP
 * x = foo() + y = bar();  and this would be naively represented
 * as a single node in a CFG; matching over function calls
 * or assignements would require then to write visitors over those nodes.
 * Even include/require constructs are represented at the expression level
 * whereas they should be really more at the toplevel or at least
 * statement level.
 * 
 * Enter PIL for PHP Intermediate Language, a better representation
 * for a PHP program. We just follow the tradition started by
 * CIL[1] and RIL[2] and propose here an AST which makes it
 * easier to write some static analysis.
 * 
 * Maybe we should focus on Ast_php because if we want to transform code
 * we have to work on that. But working on Pil can also help
 * clarify the semantic of PHP :) It could also maybe make it
 * easier to write the type inferer later.
 * 
 * 
 * We try, just like in Ast_php, to keep position information for the
 * elements in PIL, but we do it less. For instance comma_list or paren
 * stuff are removed here.
 * 
 * TODO: does the linearization respect the semantic of PHP ? Is there
 * cases where spliting a complex expression into multiple parts
 * with intermediate variables leads to a different semantic ?
 * For instance apparently $i++ is not equivalent to $i = $i + 1 when
 * $i is a string. Wonderful PHP. 
 * But at the same time our goal here is not to write a compiler. Our goal
 * is to find bugs so we maybe it's ok to relax some of the requirements.
 * 
 * References:
 *  [1] CIL, C Intermediate Language, Necula et al, CC'00
 *  [2] The Ruby Intermediate Language, Furr et al, DSL'09
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* aliases. For now we reuse the types from Ast_php but this may change. *)
type qualifier = Ast_php.qualifier
type indirect = Ast_php.indirect

type binaryOp = Ast_php.binaryOp
type unaryOp = Ast_php.unaryOp
type assignOp = Ast_php.assignOp
type castOp = Ast_php.castOp

type constant = Ast_php.constant
type class_name_reference = Ast_php.class_name_reference

type modifier = Ast_php.modifier

type name = Ast_php.name
type dname = Ast_php.dname



(* set by the type inference analysis one day *)
type type_info = {
  mutable t: Type_php.phptype;
}
 (* with tarzan *)

(* 'var' can be an existing variable (e.g. $xxx) or a fake and fresh
 * generated variable resulting from the linearization of an expression.
 *)
type var = 
  | Var of dname
  (* todo? move This in lvalue ? after all $this[2] is not possible *)
  | This of Ast_php.tok
 (* with tarzan *)

(* An 'lvalue' is what can appear at the left side of an assignement.
 *
 * Note that as opposed to Ast_php.lvalue a linearization has been done here 
 * so $o->fld1->fld2 will be transformed in $obis = $o->fld1; $obis->fld2;
 * So lvalue is not a recursive type (there is no lvalue inside
 * a lvalue).
 * Also note that functions or method calls are not there (but in 'instr').
 * 
 * TODO: have 'expr' inside ArrayAccess. Should be '(constant, var) either'
 *)
type lvalue = lvaluebis * type_info
 and lvaluebis = 
   | VVar of var
   (* A::$x. 
    * TODO change in ClassVar of qualifier * dname 
    * and introduce a DynamicClassVar of qualifier * var
    *)
   | VQualifier of qualifier * var

   | ArrayAccess of var * expr option 
   (* $o->property. Method calls are in the 'instr' type  *)
   | ObjAccess of var * name
   (* $o->$fld *)
   | DynamicObjAccess of var * var
   (* $$x *)
   | IndirectAccess of var * indirect
   (* todo: VBraceXxx ?? *)
   | TodoLvalue of Parse_info.info

(* Note that the 'expr' type is side-effect free (as opposed to Ast_php.expr).
 *  
 * Moreover this type does not contain:
 * - assignements
 * - function calls (was in the lvalue)
 * - include/require (wtf they were doing in expr anyway ...)
 * 
 * Also the constant type has been promoted one level up.
 *)
and expr = exprbis * type_info
 and exprbis =
  | Lv of lvalue

  | C of constant
  | ClassConstant of (qualifier * name)

  (* Could be grouped in a Builtin of string_op * expr list.
   * todo? do something special for short-circuit operators as in CIL ?
   *)
  | Binary  of expr * binaryOp * expr
  | Unary   of unaryOp * expr

  (* We could transform into a If but this would require to
   * change the return types of linearize_expr; this is because we would
   * need to push a stmt rather than just an instr *)
  | CondExpr of expr * expr * expr

  (* todo: apparently can put some refs & inside array() expressions *)
  (* array(expr1,..., exprn) *)
  | ConsArray of expr list
  (* array(key1 => expr1,...) also defined as an array in AST *)
  | ConsHash  of (expr * expr) list

  | Cast of castOp * expr
  | InstanceOf of expr * class_name_reference
  (* todo: remaining Sc (Guil, etc), CastUnset, *)
  | TodoExpr of Parse_info.info

 (* with tarzan *)

(* 'instr' is a kind of statement that has no local (intraprocedural)
 * control flow. It will be thus represented as a singled node in 
 * the CFG.
 * 
 * Why not 'Assign of var * expr' below to simplify even more ? 
 * Because an expr like $a[2] = 3; would be forced to be linearized into
 * $xxx_1 = $a[2]; $xxx_1 = 3; which depending on the semantic of
 * assignement could be wrong (for instance with copy-on-write semantic).
 * 
 * Right now even pure expressions or function calls where we don't
 * store the return value are convereted in assignements or call
 * where we store the return value. We could have a 'Call of lvalue option ...'
 * and and 'PureExpr of expr', but it complicates things. The current scheme
 * may lead to many dead variables, but all those variables can be detected as
 * they will be tagged as fake variables anyway.
 * 
 * TODO: not sure this is good to introduce such dead variables; it may
 * make the unused_variable checker introduce some false positives.
 *)
type instr = 
  (* $a = expr *)
  | Assign of lvalue * assign_kind * expr
  (* $a = &$b *)
  | AssignRef of lvalue * lvalue
  (* $a = foo(args) *)
  | Call   of lvalue  * call_kind * argument list
  (* eval ... bad *)
  | Eval of expr
  (* todo: Lambda *)
  | TodoInstr of Parse_info.info

  (* note: Infix and Posfix exprs were desugared into regular assigns *)
  and assign_kind = 
    | AssignEq
    | AssignOp of assignOp
    (* todo: AssignList, AssignNew, Empty? *)

  and call_kind = 
    | SimpleCall        of name
    | StaticMethodCall  of qualifier * name
    | MethodCall        of var * name
    | DynamicCall       of qualifier option * var
    | DynamicMethodCall of var * var

    | New of class_name_reference 
    (* todo: Clone *)

   and argument = 
     | Arg of expr
     | ArgRef of lvalue

 (* with tarzan *)

(* 'stmt' is very similar to Ast_php.stmt, except if/while/... contain the 
 * new 'expr' type which is side-effect free. Also we removed
 * some sugar and some deprecated forms like the ColonXxx.
 * 
 * Note that the include/require, which were removed from the 'expr'
 * are also not represented in 'stmt'. It's just bad coding. We lift them
 * up (again) to the toplevel.
 * 
 *)
type stmt = 
  | Instr of instr

  (* Could make If takes a stmt list instead of a stmt, which would remove
   * the need for Block and maybe even EmptyStmt. Not sure what is best. *)
  | Block of stmt list
  (* useful to avoid having some 'stmt option' for instance in If *)
  | EmptyStmt 

  (* elseifs are desugared; switches are transformed in ifs *)
  | If of expr * stmt * stmt
  
  (* Do, For, Foreach are desugared into a While  *)
  | While of expr * stmt

  (* todo? could transform that into gotos ? *)
  | Break of expr option
  | Continue of expr option

  | Return of expr option

  | Throw of expr
  | Try of stmt * catch

  (* InlineHtml is desugared into a regular echo *)
  | Echo of expr list

  (* todo? put expr stuff here (Print, Exit, Backquote, etc) 
   * todo? Globals, StaticVars,  Use, Unset, Declare ?
   *)
  | TodoStmt of Parse_info.info

 and catch = unit (* TODO *)

  (* with tarzan *)

type function_def = {
   f_name: name;
   f_params: parameter list;
   f_ref: bool;
   f_return_type: hint_type option;
   f_body: stmt list;
 }
  and parameter = {
    p_name: dname;
    p_type: hint_type option;
    p_ref: bool;
    p_default: static_scalar option;
  }
 (* note: 'static_scalar' used to be a separate type than 'expr' in Ast_php,
  * which by construction enforced static values. Many 'expr' constructors were
  * unfortunately duplicated in this type. So simpler to just use 'expr'.
  *)
 and static_scalar = expr
 (* we allow more places to contain a type hint when the user use the 
  * require_strict(); annotation in his code.
  *)
 and hint_type = name

  (* with tarzan *)

type class_def = {
  c_name: name;
  c_type: class_type;
  c_extends: name option;
  c_implements: name list;
  c_body: class_stmt list;
 }
 and class_type = 
   | ClassRegular | ClassFinal | ClassAbstract 
   (* interface have only the c_extends set. c_implements is always empty.
    * there was previously a 'interface_def' type but simpler to merge
    * it with class_def
    *)
   | Interface 
 and class_stmt =
   | ClassConstantDef of name * static_scalar
   | ClassVariable of 
       modifier list * (* static-php-ext: *) hint_type option *
       dname * static_scalar option
   | Method of (modifier list * function_def)
   (* the f_body will always be empty *) 
   | AbstractMethod of (modifier list * function_def)
  (* with tarzan *)

type require = unit (* TODO *)

type toplevel = 
  | Require of require
  | TopStmt of stmt
  | FunctionDef of function_def
  (* actually also used also for interface *)
  | ClassDef of class_def

  (* with tarzan *)

type program = toplevel list

  (* with tarzan *)

(*e: pil.ml *)
