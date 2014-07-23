(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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
(* An Abstract Syntax Tree for a mini C.
 * 
 * This will help prototype program analysis by not having to deal with
 * the enormity of a real programming language. So now have ast_cpp.ml
 * -> ast_c.ml -> ast_minic.ml. 
 * 
 * Here is a list of the simplications compared to ast_c.ml:
 * - types: no unions, no typedefs, no enum,
 *   todo? no array?
 * - exprs: no infix, postfix, and introduce intermediate instr type
 *   so have already put in some kind of A-Normal form.
 * 
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type 'a wrap = 'a * Parse_info.info

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* for functions, fields *)
type name = string wrap

(* for local variables, globals, parameters *)
type var = name

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

type type_ =
  | TBase of name (* int, string *)
  | TPointer of type_
  | TFunction of function_type
  | TStructName of name

 and function_type = (type_ * parameter list)
  and parameter = {
    p_type: type_;
    p_name: var;
  }

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
  | Int of string wrap
  | String of string wrap

  | Var of var

(* ------------------------------------------------------------------------- *)
(* Stmt *)
(* ------------------------------------------------------------------------- *)

(* todo: pointers??? *)
type instr =
  | Alloc of var * type_ (* var = malloc(sizeof(type)) *)
  | Move of var * var    (* to = from *)
  | LoadField of var *  var * name (* to = base.fld *)
  | StoreField of var * name   * var (* base.fld = from *)
  | StaticCall of var * name * expr list (* to = foo(...) *)
  | DynamicCall of var * var * expr list (* to = ( *var)(...) *)

type stmt =
  | Instr of instr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | Return of expr

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)

type var_decl = {
  v_name: name;
  v_type: type_;
}

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

type func_def = {
  f_name: name;
  f_type: function_type;
  f_body: stmt list;
}


type struct_def = {
  s_name: name;
  s_flds: field_def list;
}

  and field_def = { 
    fld_name: name option;
    fld_type: type_;
  }

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type toplevel =
  | StructDef of struct_def
  | FuncDef of func_def
  | Global of var_decl

type program = toplevel list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
