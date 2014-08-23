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
 * - exprs: no infix, postfix, no -> vs ., just ->
 * - stmt: introduce intermediate instr type so have restricted forms of
 *   assigments
 * - ...
 * 
 * Essentially a restricted set of C programs in A-normal form.
 * 
 * related work:
 *   - CIL, C Intermediate Language, Necula et al, CC'00
 *   - The Ruby Intermediate Language, Furr et al, DSL'09
 *   - my own PIL for PHP
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type 'a wrap = 'a * Parse_info.info

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* for functions, constants, fields, builtins, types *)
type name = string wrap

(* for globals, locals, parameters *)
type var = name

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)

type type_ =
  | TBase of name (* int, string *)
  | TPointer of type_
  | TArray of type_
  | TFunction of function_type
  | TStructName of name

 and function_type = (type_ * parameter list)
  and parameter = var_decl

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr =
  | Int of string wrap
  | String of string wrap
  | Id of name (* can be a global, local, parameter, constant, functions *)

  | DeRef of var (*  *x *)

  | Alloc of type_ (* malloc(sizeof(type)) *)
  | AllocArray of var * type_ (* malloc(n*sizeof(type)) *)
  | ObjField of var * name (* x->fld *)
  | ArrayAccess of var * var (* x[y] *)
  | StaticCall of name * var list (* foo(...) *)
  | DynamicCall of var * var list (* ( *f)(...) *)
  | BuiltinCall of name * var list (* e.g. v + 1 *)

(* ------------------------------------------------------------------------- *)
(* Stmt *)
(* ------------------------------------------------------------------------- *)

(* less: actually datalog_c.ml introduce lvalue/rvalue which is cleaner *)
and instr =
  | Assign of var * expr (* x = e *)
  | AssignAddress of var * name (* x = &v *) (* of global, local, param, func *)
  | AssignDeref of var * var (* *x = v *)

  | AssignField of var * name * var (* x->f = v *)
  | AssignArray of var * var * var (* x[y] = v *)
  | AssignFieldAddress of var * var * name (* x = &v->field *)
  | AssignIndexAddress of var * var * var (* x = &v[y] *)



and stmt =
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
  v_type: type_;
}

type func_def = {
  f_name: name;
  f_type: function_type;
  f_body: stmt list;
}


type struct_def = {
  s_name: name;
  s_flds: field_def list;
}
  and field_def = var_decl

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type toplevel =
  | StructDef of struct_def
  | FuncDef of func_def
  | Global of var_decl
  | Constant of name

type program = toplevel list

