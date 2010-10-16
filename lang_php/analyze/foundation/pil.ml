(*s: pil.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
 * CIL[1] and RIL[2] and propose here an AST bis which makes it
 * easier to write some static analysis.
 * 
 * Maybe I should focus on Ast_php because if we want to transform code
 * we have to work on that. But working on Pil can also help
 * clarify my comprehension of PHP :) It could also maybe make it
 * easier to write the type inferer later.
 * 
 * 
 * I try, just like in Ast_php, to keep position information for the
 * elements in PIL, but I do it less. For instance comma_list or paren
 * stuff are removed here.
 * 
 * TODO: does the linearization respect the semantic of PHP ? Is there
 * cases where spliting a complex expression into multiple parts
 * with intermediate variables leads to a different semantic ?
 * For instance apparently $i++ is not equivalent to $i = $i + 1 when
 * $i is a string. Wonderful PHP. But at the same time our goal here is
 * not to write a compiler. Our goal is to find bugs so we may relax some
 * requirements.
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

type name = Ast_php.name
type dname = Ast_php.dname



(* set by the type inference analysis *)
type type_info = {
  mutable t: Type_php.phptype;
}
 (* with tarzan *)

(* 'var' can be an existing variable (e.g. $xxx) or a fake and fresh
 * generated variable resulting from the linearization of an expression.
 *)
type var = 
  | Var of dname
  (* todo? move in lvalue ? after all $this[2] is not possible *)
  | This of Ast_php.tok
 (* with tarzan *)

(* 'lvalue', what can appear at the left side of an assignement. 
 *
 * Note that as opposed to Ast_php.lvalue a linearization has been done so
 * $o->fld1->fld2 will be transformed in $obis = $o->fld1; $obis->fld2;
 * So lvalue is not a a recursive type (there is no lvalue inside
 * a lvalue). 
 * Also note that function or method calls are not there (but in 'instr').
 * 
 * TODO: BAD have expr inside ArrayAccess. Should be '(constant, var) either'
 *)
type lvalue = lvaluebis * type_info
 and lvaluebis = 
   | VVar of var
   (* A::$x *)
   | VQualifier of qualifier * var

   | ArrayAccess of var * expr option 
   (* $o->property. Method calls are in the 'instr' type  *)
   | ObjAccess of var * name
   (* $o->$fld *)
   | DynamicObjAccess of var * var
   (* $$x *)
   | IndirectAccess of var * indirect
   (* todo: VBraceXxx ?? *)

(* 'expr' is side effect free (as opposed to Ast_php.expr) ! 
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

 (* with tarzan *)

(* 'instr' is a statement that has no local (intraprocedural) 
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
 * new 'expr' type which is side-effect free. Also removed
 * some sugar and some deprecated forms like the ColonXxx.
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

  (* elseifs are desugared; Switch are transformed in ifs *)
  | If of expr * stmt * stmt
  
  (* Do, For, Foreach are desugared into a While  *)
  | While of expr * stmt

  (* todo: could transform that into gotos ? *)
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

 and catch = unit (* TODO *)
  (* with tarzan *)

type toplevel = 
  | Require of require
  | TopStmt of stmt

  | FunctionDef of function_def
  | ClassDef of class_def
  | InterfaceDef of interface_def

 and function_def = unit (* TODO *)
 and class_def = unit (* TODO *)
 and interface_def = unit (* TODO *)
 and require = unit (* TODO *)
  (* with tarzan *)

type program = toplevel list
  (* with tarzan *)

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

(* I wish I had deriving Show in OCaml ... in the mean time I have
 * to use my ocamltarzan tool to do compile-time reflection.
 *)

(* generated by ocamltarzan: ocamltarzan -vof pil.ml *)
let vof_dname = Meta_ast_php.vof_dname
let vof_name = Meta_ast_php.vof_name
let vof_qualifier = Meta_ast_php.vof_qualifier

let vof_indirect = Meta_ast_php.vof_indirect
let vof_binaryOp = Meta_ast_php.vof_binaryOp
let vof_unaryOp = Meta_ast_php.vof_unaryOp
let vof_assignOp = Meta_ast_php.vof_assignOp
let vof_castOp = Meta_ast_php.vof_castOp

let vof_constant = Meta_ast_php.vof_constant
let vof_class_name_reference = Meta_ast_php.vof_class_name_reference

let vof_phptype x = Ocaml.VTODO "type"

let vof_type_info { t = v_t } =
  let bnds = [] in
  let arg = vof_phptype v_t in
  let bnd = ("t", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds


let vof_var =
  function
  | Var v1 -> let v1 = vof_dname v1 in Ocaml.VSum (("Var", [ v1 ]))
  | This v1 -> let v1 = Meta_ast_php.vof_tok v1 in Ocaml.VSum (("This", [ v1 ]))
  
let rec vof_lvalue (v1, v2) =
  let v1 = vof_lvaluebis v1
  and v2 = vof_type_info v2
  in Ocaml.VTuple [ v1; v2 ]
and vof_lvaluebis =
  function
  | VVar v1 -> let v1 = vof_var v1 in Ocaml.VSum (("VVar", [ v1 ]))
  | VQualifier ((v1, v2)) ->
      let v1 = vof_qualifier v1
      and v2 = vof_var v2
      in Ocaml.VSum (("VQualifier", [ v1; v2 ]))
  | ArrayAccess ((v1, v2)) ->
      let v1 = vof_var v1
      and v2 = Ocaml.vof_option vof_expr v2
      in Ocaml.VSum (("ArrayAccess", [ v1; v2 ]))
  | ObjAccess ((v1, v2)) ->
      let v1 = vof_var v1
      and v2 = vof_name v2
      in Ocaml.VSum (("ObjAccess", [ v1; v2 ]))
  | DynamicObjAccess ((v1, v2)) ->
      let v1 = vof_var v1
      and v2 = vof_var v2
      in Ocaml.VSum (("DynamicObjAccess", [ v1; v2 ]))
  | IndirectAccess ((v2, v1)) ->
      let v1 = vof_indirect v1
      and v2 = vof_var v2
      in Ocaml.VSum (("IndirectAccess", [ v2; v1 ]))
and vof_expr (v1, v2) =
  let v1 = vof_exprbis v1
  and v2 = vof_type_info v2
  in Ocaml.VTuple [ v1; v2 ]
and vof_exprbis =
  function
  | Lv v1 -> let v1 = vof_lvalue v1 in Ocaml.VSum (("Lv", [ v1 ]))
  | C v1 ->
      let v1 = vof_constant v1 in Ocaml.VSum (("C", [ v1 ]))
  | Binary ((v1, v2, v3)) ->
      let v1 = vof_expr v1
      and v2 = vof_binaryOp v2
      and v3 = vof_expr v3
      in Ocaml.VSum (("Binary", [ v1; v2; v3 ]))
  | Unary ((v1, v2)) ->
      let v1 = vof_unaryOp v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("Unary", [ v1; v2 ]))
  | CondExpr ((v1, v2, v3)) ->
      let v1 = vof_expr v1
      and v2 = vof_expr v2
      and v3 = vof_expr v3
      in Ocaml.VSum (("CondExpr", [ v1; v2; v3 ]))
  | ConsArray v1 ->
      let v1 = Ocaml.vof_list vof_expr v1
      in Ocaml.VSum (("ConsArray", [ v1 ]))
  | ConsHash v1 -> 
      let v1 = Ocaml.vof_list 
               (fun (e2, e3) ->
                  let v2 = vof_expr e2
                  and v3 = vof_expr e3 in
                Ocaml.VTuple [ v2; v3 ]) v1
      in Ocaml.VSum (("ConsHash", [ v1 ]))
  | Cast ((v1, v2)) ->
      let v1 = vof_castOp v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("Cast", [ v1; v2 ]))
  | InstanceOf ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_class_name_reference v2
      in Ocaml.VSum (("InstanceOf", [ v1; v2 ]))
  
let rec vof_instr =
  function
  | Assign ((v1, v2, v3)) ->
      let v1 = vof_lvalue v1
      and v2 = vof_assign_kind v2
      and v3 = vof_expr v3
      in Ocaml.VSum (("Assign", [ v1; v2; v3 ]))
  | AssignRef ((v1, v2)) ->
      let v1 = vof_lvalue v1
      and v2 = vof_lvalue v2
      in Ocaml.VSum (("AssignRef", [ v1; v2 ]))
  | Call ((v1, v2, v3)) ->
      let v1 = vof_lvalue v1
      and v2 = vof_call_kind v2
      and v3 = Ocaml.vof_list vof_argument v3
      in Ocaml.VSum (("Call", [ v1; v2; v3 ]))
  | Eval v1 -> let v1 = vof_expr v1 in Ocaml.VSum (("Eval", [ v1 ]))
and vof_assign_kind =
  function
  | AssignEq -> Ocaml.VSum (("AssignEq", []))
  | AssignOp v1 ->
      let v1 = vof_assignOp v1 in Ocaml.VSum (("AssignOp", [ v1 ]))
and vof_call_kind =
  function
  | SimpleCall v1 ->
      let v1 = vof_name v1 in Ocaml.VSum (("SimpleCall", [ v1 ]))
  | StaticMethodCall ((v1, v2)) ->
      let v1 = vof_qualifier v1
      and v2 = vof_name v2
      in Ocaml.VSum (("StaticMethodCall", [ v1; v2 ]))
  | MethodCall ((v1, v2)) ->
      let v1 = vof_var v1
      and v2 = vof_name v2
      in Ocaml.VSum (("MethodCall", [ v1; v2 ]))
  | DynamicCall ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_qualifier v1
      and v2 = vof_var v2
      in Ocaml.VSum (("DynamicCall", [ v1; v2 ]))
  | DynamicMethodCall ((v1, v2)) ->
      let v1 = vof_var v1
      and v2 = vof_var v2
      in Ocaml.VSum (("DynamicMethodCall", [ v1; v2 ]))
  | New v1 ->
      let v1 = vof_class_name_reference v1 in Ocaml.VSum (("New", [ v1 ]))
and vof_argument =
  function
  | Arg v1 -> let v1 = vof_expr v1 in Ocaml.VSum (("Arg", [ v1 ]))
  | ArgRef v1 -> let v1 = vof_lvalue v1 in Ocaml.VSum (("ArgRef", [ v1 ]))
  
let rec vof_stmt =
  function
  | Instr v1 -> let v1 = vof_instr v1 in Ocaml.VSum (("Instr", [ v1 ]))
  | Block v1 ->
      let v1 = Ocaml.vof_list vof_stmt v1 in Ocaml.VSum (("Block", [ v1 ]))
  | EmptyStmt -> Ocaml.VSum (("EmptyStmt", []))
  | If ((v1, v2, v3)) ->
      let v1 = vof_expr v1
      and v2 = vof_stmt v2
      and v3 = vof_stmt v3
      in Ocaml.VSum (("If", [ v1; v2; v3 ]))
  | While ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_stmt v2
      in Ocaml.VSum (("While", [ v1; v2 ]))
  | Break v1 ->
      let v1 = Ocaml.vof_option vof_expr v1 in Ocaml.VSum (("Break", [ v1 ]))
  | Continue v1 ->
      let v1 = Ocaml.vof_option vof_expr v1
      in Ocaml.VSum (("Continue", [ v1 ]))
  | Return v1 ->
      let v1 = Ocaml.vof_option vof_expr v1
      in Ocaml.VSum (("Return", [ v1 ]))
  | Throw v1 -> let v1 = vof_expr v1 in Ocaml.VSum (("Throw", [ v1 ]))
  | Try ((v1, v2)) ->
      let v1 = vof_stmt v1
      and v2 = vof_catch v2
      in Ocaml.VSum (("Try", [ v1; v2 ]))
  | Echo v1 ->
      let v1 = Ocaml.vof_list vof_expr v1 in Ocaml.VSum (("Echo", [ v1 ]))
and vof_catch v = Ocaml.vof_unit v

let rec vof_toplevel =
  function
  | Require v1 -> let v1 = vof_require v1 in Ocaml.VSum (("Require", [ v1 ]))
  | TopStmt v1 -> let v1 = vof_stmt v1 in Ocaml.VSum (("TopStmt", [ v1 ]))
  | FunctionDef v1 ->
      let v1 = vof_function_def v1 in Ocaml.VSum (("FunctionDef", [ v1 ]))
  | ClassDef v1 ->
      let v1 = vof_class_def v1 in Ocaml.VSum (("ClassDef", [ v1 ]))
  | InterfaceDef v1 ->
      let v1 = vof_interface_def v1 in Ocaml.VSum (("InterfaceDef", [ v1 ]))
and vof_function_def v = Ocaml.vof_unit v
and vof_class_def v = Ocaml.vof_unit v
and vof_interface_def v = Ocaml.vof_unit v
and vof_require v = Ocaml.vof_unit v
  
let vof_program v = Ocaml.vof_list vof_toplevel v


(*****************************************************************************)
(* String of *)
(*****************************************************************************)
type debug_config = {
  show_types: bool;
  show_tokens: bool;
}

let default_debug_config = {
  show_types = false;
  show_tokens = false;
}

let (adjust_ocaml_v: ?config:debug_config -> Ocaml.v -> Ocaml.v) =
 fun ?(config=default_debug_config) v ->

  let v = 
    Ocaml.map_v ~f:(fun ~k v ->
      match v with
      | Ocaml.VDict xs ->
         (* let's hide all those {pinfo = ... } and type annotations *)
          if 
            (xs +> List.exists (fun (s, _) -> s = "pinfo") &&
             not config.show_tokens
             ) ||
            (xs +> List.exists (fun (s, _) -> s = "t") &&
             not (config.show_types
             ))
          then Ocaml.VDict []
          else k v
      | _ -> k v
    ) v
  in
  v

let string_of_instr ?config x =
  x +> vof_instr +> adjust_ocaml_v ?config +> Ocaml.string_of_v

let string_of_stmt ?config x = 
  x +> vof_stmt +> adjust_ocaml_v ?config +> Ocaml.string_of_v

let string_of_expr ?config x = 
  x +> vof_expr +> adjust_ocaml_v ?config +> Ocaml.string_of_v


let string_of_program ?config x =
  x +> vof_program +> adjust_ocaml_v ?config +> Ocaml.string_of_v
  

(*e: pil.ml *)
