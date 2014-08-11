(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
open Common2.Infix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * A (real) Abstract Syntax Tree for C, not a Concrete Syntax Tree
 * as in ast_cpp.ml.
 * 
 * This file contains a simplified C abstract syntax tree. The original
 * C/C++ syntax tree (ast_cpp.ml) is good for code refactoring or
 * code visualization; the type used matches exactly the source. However,
 * for other algorithms, the nature of the AST makes the code a bit
 * redundant. Moreover many analysis are far simpler to write on
 * C than C++. Hence the idea of a SimpleAST which is the
 * original AST where certain constructions have been factorized
 * or even removed.

 * Here is a list of the simplications/factorizations:
 *  - no C++ constructs, just plain C
 *  - no purely syntactical tokens in the AST like parenthesis, brackets, 
 *    braces, commas, semicolons, etc. No ParenExpr. No FinalDef. No
 *    NotParsedCorrectly. The only token information kept is for identifiers
 *    for error reporting. See name below.
 *  - ...
 *  - no nested struct, they are lifted to the toplevel
 *  - no mix of typedef with decl
 *  - sugar is removed, no RecordAccess vs RecordPtAccess, ...
 *  - no init vs expr
 *  - no Case/Default in statement but instead a focused 'case' type
 * 
 * todo: ast_c_simple_build.ml is probably very incomplete, but for now
 * good enough for codegraph purposes on xv6, plan9 and other small C
 * projects.
 * 
 * related: 
 *  - CIL
 * 
 * See lang_cpp/parsing/ast_cpp.ml.
 *
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type 'a wrap = 'a * Ast_cpp.tok

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type name = string wrap
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

(* less: qualifier (const/volatile) *)
type type_ =
  | TBase of name (* int, float, etc *)
  | TPointer of type_
  | TArray of type_
  | TFunction of function_type
  | TStructName of struct_kind * name
  (* hmmm but in C it's really like an int no? *)
  | TEnumName of name
  | TTypeName of name

 and function_type = (type_ * parameter list)

  and parameter = {
    p_type: type_;
    (* when part of a prototype, the name is not mentionned *)
    p_name: name option;
  }

 and struct_kind = Struct | Union
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
  | Int of string wrap
  | Float of string wrap
  | String of string wrap
  | Char of string wrap

  (* can be a cpp or enum constant (e.g. FOO), or a local/global variable, 
   * or a function.
   *)
  | Id of name

  | Call of expr * argument list

  | Assign of Ast_cpp.assignOp * expr * expr

  | ArrayAccess of expr * expr
  | RecordAccess of expr * name

  | Cast of type_ * expr

  (* todo? transform into Call (builtin ...) ? *)
  | Postfix of expr * Ast_cpp.fixOp
  | Infix of expr * Ast_cpp.fixOp
  (* contains GetRef and Deref!! todo: lift up? *)
  | Unary of expr * Ast_cpp.unaryOp
  | Binary of expr * Ast_cpp.binaryOp * expr

  | CondExpr of expr * expr * expr
  (* should be a statement *)
  | Sequence of expr * expr

  | SizeOf of (expr, type_) Common.either

  (* should appear only as part of a variable initializer *)
  | InitList of expr list
  (* gccext: *)
  | GccConstructor  of type_ * expr (* always an InitList *)
 (* with tarzan *)

and argument = expr

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
type stmt = 
  | ExprSt of expr
  | Block of stmt list

  | If of expr * stmt * stmt
  | Switch of expr * case list

  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of expr option * expr option * expr option * stmt

  | Return of expr option
  | Continue | Break

  | Label of name * stmt
  | Goto of name

  | Vars of var_decl list
  (* todo: it's actually a special kind of format, not just an expr *)
  | Asm of expr list

  and case =
    | Case of expr * stmt list
    | Default of stmt list

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)

and var_decl = {
  v_name: name;
  v_type: type_;
  (* todo *)
  v_storage: storage;
  v_init: initialiser option;
}

 and initialiser = expr
 and storage = Extern | Static | DefaultStorage

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

type func_def = {
  f_name: name;
  f_type: function_type;
  f_body: stmt list;
}
 (* with tarzan *)


type struct_def = {
  s_name: name;
  s_kind: struct_kind;
  s_flds: field_def list;
}
  (* less: could merge with var_decl, but field have no storage normally.
   * todo: bitfield annotation
   * kencc-ext: the option on fld_name can be a kencc extension
   * or a bitfield.
   *)
  and field_def = { 
    fld_name: name option;
    fld_type: type_;
  }
 (* with tarzan *)

(* todo: use a record *)
type enum_def = name * (name * expr option) list
 (* with tarzan *)

(* todo: use a record *)
type type_def = name * type_
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Cpp *)
(* ------------------------------------------------------------------------- *)

(* should be a statically computable expr *)
type define_body = 
  | CppExpr of expr
  | CppStmt of stmt
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
(* less: ForwardStructDecl for mutually recursive structures? probably can 
 * deal with it by using typedefs as intermediates.
 *)
type toplevel =
  | Include of string wrap (* path *)
  | Define of name * define_body 
  | Macro of name * (name list) * define_body

  | StructDef of struct_def
  | TypeDef of type_def
  | EnumDef of enum_def

  | FuncDef of func_def
  | Global of var_decl (* also contain extern decl *)
  | Prototype of func_def (* empty body *)
 (* with tarzan *)

type program = toplevel list
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Type of type_
  | Toplevel of toplevel
  | Program of program
 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* builtin() is used for:
 *)
let builtin x = "__builtin__" ^ x

let str_of_name (s, _) = s

let looks_like_macro name =
  let s = str_of_name name in
  s =~ "[A-Z][A-Z_0-9]*"
