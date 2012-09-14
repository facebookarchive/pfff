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
open Common.Infix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * A (real) Abstract Syntax Tree for C, not a Concrete Syntax Tree
 * as in ast_cpp.ml.
 * 
 * This file contains a simplified C abstract syntax tree. The original
 * C++ syntax tree (ast_cpp.ml) is good for code refactoring or
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
 * 
 *
 * related: 
 *  - CIL
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type 'a wrap = 'a * Ast_cpp.tok

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type name = string wrap

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

(* todo: qualifier *)
type type_ =
  | TBase of name
  | TPointer of type_
  | TArray of type_
  | TFunction of function_type
  | TStructName of name
  | TTypeName of name

 and function_type = (type_ * parameter list)

  and parameter = {
    p_type: type_;
    p_name: name;
  }

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
  | Int of string wrap
  | Float of string wrap
  | String of string wrap
  | Char of string wrap

  (* can be a cpp constant XXX, or a variable, or a function *)
  | Id of name

  | Call of expr * expr list

  | Assign of Ast_cpp.assignOp option * expr * expr

  | ArrayAccess of expr * expr
  | RecordAccess of expr * name

  | Cast of type_ * expr

  (* todo? transform into Call (builtin ...) ? *)
  | Postfix of expr * Ast_cpp.fixOp
  | Infix of expr * Ast_cpp.fixOp
  | Unary of expr * Ast_cpp.unaryOp
  | Binary of expr * Ast_cpp.binaryOp * expr

  | CondExpr of expr * expr * expr
  (* should be a statement *)
  | Sequence of expr * expr

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
type stmt = 
  | Expr of expr
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

  | Decl of var_decl

  and case =
    | Case of expr * stmt list
    | Default of stmt list

and var_decl = {
  v_name: name;
  v_type: type_;
  (* todo *)
  v_storage: unit;
  v_init: expr option;
}

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

type func_def = {
  f_name: name;
  f_body: stmt list;
  f_type: function_type;
}

type struct_def = {
  s_name: name;
  s_kind: struct_kind;
  s_flds: field_def list;
}
  and struct_kind = Struct | Union

  (* todo? merge with var_decl? *)
  and field_def = { 
    fld_name: name;
    fld_type: type_;
  }

(* ------------------------------------------------------------------------- *)
(* Cpp *)
(* ------------------------------------------------------------------------- *)
type define_body = expr

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
type toplevel =
  | Define of name * define_body (* should be a statically computable expr *)
  | Include of string wrap (* path *)
  | Macro of name * (name list) * define_body

  | StructDef of struct_def
  | TypeDef of name * type_

  | FuncDef of func_def
  | Global of var_decl
  | Prototype of func_def (* empty body *)


type program = toplevel list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_name (s, _) = s

let looks_like_macro name =
  let s = str_of_name name in
  s =~ "[A-Z][A-Z_0-9]*"
