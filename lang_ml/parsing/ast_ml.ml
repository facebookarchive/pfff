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

open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type info = Parse_info.info
and tok = info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * info

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok 

and 'a comma_list = ('a, tok (* ',' *)) Common.either list
and 'a and_list = ('a, tok (* 'and' *)) Common.either list

and 'a semicolon_list = ('a, tok (* ';' *)) Common.either list
and 'a star_list = ('a, tok (* '*' *)) Common.either list

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)
type name = Name of string wrap

(* lower and uppernames aliases, just for clarity *)
and lname = name
and uname = name

 (* with tarzan *)

type long_name = qualifier * name
 and qualifier = (name * tok (*'.'*)) list

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(* core language, module language, class language *)

type ty = 
  | TyName of long_name
  | TyVar of tok * name
  | TyTuple of ty star_list (* at least 2 *)
  | TyFunction of ty * tok (* -> *) * ty
  | TyApp of ty_args * long_name (* could be merged wit TyName *)

  | TyTodo

and ty_args = 
  | TyArg1 of ty
  | TyArgMulti of ty comma_list paren
  (* TyNoArg and merge TyName and TyApp ? *)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)
and expr = unit

(* can have an optional ';' as terminator too, not only separator *)
and seq_expr = expr semicolon_list

(* ------------------------------------------------------------------------- *)
(* Patterns *)
(* ------------------------------------------------------------------------- *)
and pattern = unit

and simple_pattern = unit

(* rename in parameter ? *)
and labeled_simple_pattern = unit

(* ------------------------------------------------------------------------- *)
(* Type declarations *)
(* ------------------------------------------------------------------------- *)
and type_declaration = unit

and constructor_arguments =
  | NoConstrArg
  | Of of tok * ty star_list

(* ------------------------------------------------------------------------- *)
(* Let binding *)
(* ------------------------------------------------------------------------- *)

and let_binding =
  | LetClassic of let_def
  | LetPattern of pattern * tok (* = *) * seq_expr

(* was called fun_binding in the grammar *)
and let_def = {
  l_name: name; (* val_ident *)
  l_args: labeled_simple_pattern list; (* can be empty *)
  l_tok: tok; (* = *)
  l_body: seq_expr;
 }

(* ------------------------------------------------------------------------- *)
(* Function binding *)
(* ------------------------------------------------------------------------- *)
 
and function_def = unit

(* ------------------------------------------------------------------------- *)
(* Module *)
(* ------------------------------------------------------------------------- *)
and module_type = unit

and module_expr = unit
(* ------------------------------------------------------------------------- *)
(* Class *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Signature/Structure items *)
(* ------------------------------------------------------------------------- *)

(* could split in sig_item and struct_item but many constructions are
 * valid in both contexts.
 *)
and item = 
 | TypeDecl of tok * type_declaration and_list
 | ExceptionDecl of tok * name * constructor_arguments
 | ExternalDecl of tok * name (* val_ident *) * tok (*:*) * ty * tok (* = *) *
     string wrap list (* primitive declarations *)

 | Open of tok * long_name

 (* only in sig_item *)
 | ValDecl of tok * name (* val_ident *) * tok (*:*) * ty

 (* only in struct_item *)
 | Let of tok * rec_opt * let_binding and_list
     
 | ItemTodo

and sig_item = item
and struct_item = item

  and rec_opt = tok option

(* ------------------------------------------------------------------------- *)
(* Toplevel phrases *)
(* ------------------------------------------------------------------------- *)

and toplevel =
  | Item of item

  (* should both be removed *)
  | ScSc of info (* ;; *)
  | TopSeqExpr of seq_expr

  | NotParsedCorrectly of info list
  | FinalDef of info (* EOF *)

  | TODO of info

 and program = toplevel list

 (* with tarzan *)


(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let str_of_info x = Parse_info.str_of_info x
let col_of_info x = Parse_info.col_of_info x
let line_of_info x = Parse_info.line_of_info x
let pos_of_info x = Parse_info.pos_of_info x
