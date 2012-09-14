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
 *  - no nested struct, they are lifter at the toplevel
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

type type_ =
  | TBuiltin of name

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
  | Int of string wrap
  | Float of string wrap
  | String of string wrap

  (* can be a cpp constant XXX, or a variable, or a function *)
  | Id of name

  | Call of expr * expr list

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
type stmt = 
  | Expr of expr

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

type func_def = {
  f_name: name;
  f_body: stmt list;
}
  and parameter = {
    p_type: type_;
    p_name: name;
  }

type struct_def = {
  s_name: name;
  s_flds: field_def list;
}
  (* todo? merge with var_decl? *)
  and field_def = { 
    fld_name: name;
    fld_type: type_;
  }

type var_decl = {
  v_name: name;
  v_type: type_;
}

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
type toplevel =
  (* no Ifdef *)
  | Define of name * expr (* should be a statically computable expr *)
  | Include of string wrap (* path *)

  | StructDef of struct_def
  | FuncDef of func_def
  | Global of var_decl
  | TypeDef of name * type_

type program = toplevel list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
