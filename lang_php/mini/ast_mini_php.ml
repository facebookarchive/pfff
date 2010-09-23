(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
(* Mini PHP type *)
(*****************************************************************************)

type phptype = phptypebis list (* the list is the union *)
 and phptypebis =

   | TBool
   | TNum
   | TString

   | TUnit

   | TArray of phptype
   | THash  of phptype
   | TRecord of (string * phptype) list
       
   | TNull

   | TVariant (* a.k.a Top *)
   | TUnknown (* a.k.a Bottom *)
       
   | TypeVar of string
 (* with sexp *)

(*****************************************************************************)
(* Mini AST *)
(*****************************************************************************)

type expr = exprbis * expr_info 
  and expr_info = {
   mutable t: phptype;
  }
 and exprbis =
   | Bool of bool
   | Number of string
   | String of string
   | Null

   | Var of string 

   | ArrayAccess of expr * expr
   | Assign of expr * expr

   | Binary of expr * Ast_php.binaryOp * expr

   | Funcall of string * expr list

and stmt = 
  (* todo: lift funcall here, in instr, as well as assign, as in PIL *)
  | ExprStmt of expr
  | Echo of expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Block of stmt list
  | Return of expr option


and toplevel = 
  | FuncDef of string * (string * expr option) list * stmt list
  | StmtList of stmt list

and program = toplevel list
 (* with sexp *)
