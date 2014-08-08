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
(* An Abstract Syntax Tree for a mini Java.
 * 
 * This will help prototype program analysis by not having to deal with
 * the enormity of a real programming language.
 * Why not use Mini C? because mini C has pointers which complicates
 * things. Java has classes and references but they are less complicated
 * for program analysis to handle.
 * 
 * Here is a list of the simplications compared to ast_java.ml:
 *  - no packages
 *  - no generics, no annotations, no enum
 *  - no arrays (a special case of generic collections anyway)
 *  - no modifiers
 *  - ...
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type 'a wrap = 'a * Parse_info.info

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* for classes, methods, fields *)
type name = string wrap

(* for local variables, parameters (for globals use class static constants) *)
type var = name

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

type typ =
  | TBase of name (* Integer, String *)
  | TClassName of name

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

type expr =
  | Int of string wrap
  | String of string wrap

  | Var of var
  | New of name (* args? *)
  | ObjField of var * name
  | ClassField of name * name
  | ObjCall of var * name * var list
  | ClassCall of name * name * var list

type instr = 
  | Assign of var * expr
  | AssignField of var * name * var
  | AssignClassField of name * name * var


type stmt =
  | Local of var_decl
  | Instr of instr
  | If of var * stmt list * stmt list
  | While of var * stmt list
  | Return of var

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)

and var_decl = {
  v_name: name;
  v_type: typ;
}

type method_decl = {
  (* name and ret type could be gathered in a m_var: var_decl *)
  m_name: name;
  m_ret: typ;
  m_static: bool;
  m_formals: var list;
  m_body: stmt list;
}

type field_decl = var_decl * bool (* static field *)

type class_decl = {
  cl_name: name;
  cl_fields: field_decl list;
  cl_methods: method_decl list;
}


(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type program = class_decl list
