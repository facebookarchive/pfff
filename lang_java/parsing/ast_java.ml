(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License 
 * 
 * Yoann Padioleau: 
 * 2010, port to the pfff infrastructure.
 * 2012, support annotation and generics
 *)

module PI = Parse_info
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A simple AST for Java.
 * 
 * TODO: 
 *  - support annotations
 *  - support for generics
 *  - support enums
 *  - etc.
 *)

(*****************************************************************************)
(* The AST java related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
(* forunparser: *)
type info = Parse_info.info

(* todo:  = 'a * info *)
and 'a wrap  = 'a * info list

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
and ident = string wrap (* could do a wrap3 where wrap3 = just 1 info *)

and name = ident (*wrap2 '.' *) list

and names = name list

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

and typ =
  | TypeName of name (* include 'void', 'int', and other primitive types *)
  | ArrayType of typ

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

and expr =
  | Name of name (* include 'this' and 'super' special names *)

  (* todo: split in constant type with Int | Float | String | Char | Bool *)
  | Literal of string

  | ClassLiteral of typ

  | NewClass of typ * exprs * decls wrap (* { } *) option
  | NewQualifiedClass of expr * ident * exprs * decls wrap (* { } *) option
  | NewArray of typ * exprs * int * init option

  | Dot of expr * ident
  (* less: split in MethodCallSimple and so on? hmmm maybe not. *)
  | Call of expr * exprs
  | ArrayAccess of expr * expr

  | Postfix of expr * op
  | Prefix of op * expr
  | Cast of typ * expr
  | Infix of expr * op * expr

  | InstanceOf of expr * typ

  | Conditional of expr * expr * expr
  (* ugly java, as C, assignement is an expression not a statement :( *)
  | Assignment of expr * op * expr

and exprs = expr list

and op = string

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)

and stmt = stmtbis wrap
 and stmtbis =
  | Empty

  | Block of stmts

  (* expr but in grammar restricted to side-effect expr, so good *)
  | Expr of expr

  | If of expr * stmt * stmt option
  | Switch of expr * (cases * stmts) list


  | While of expr * stmt
  | Do of stmt * expr
  | For of for_control * stmt

  | Break of ident option
  | Continue of ident option
  | Return of expr option
  | Label of ident * stmt

  | Sync of expr * stmt

  | Try of stmt * catches * stmt option
  | Throw of expr

  (* decl as statement *)
  | LocalVar of field

  | LocalClass of class_decl

  (* javaext: http://java.sun.com/j2se/1.4.2/docs/guide/lang/assert.html *)
  | Assert of expr * expr option (* assert e or assert e : e2 *)

and stmts = stmt list

and case = casebis wrap
 and casebis = 
  | Case of expr
  | Default
and cases = case list

and for_control = unit

and catch = var * stmt
and catches = catch list

(* ------------------------------------------------------------------------- *)
(* Variable declaration *)
(* ------------------------------------------------------------------------- *)

and modifier = modifierbis wrap (* could do wrap3 instead *)
 and modifierbis =
  | Public   | Protected   | Private
  | Abstract
  | Static 
  | Final
  | StrictFP
  | Transient   | Volatile
  | Synchronized
  | Native

  | Annotation

and modifiers = modifier list

and vars = var list

and var = { 
  v_mods: modifiers;
  v_type: typ;
  v_name: ident 
}

and init = initbis wrap
  and initbis =
  | ExprInit of expr
  | ArrayInit of init list

(* ------------------------------------------------------------------------- *)
(* Method, field *)
(* ------------------------------------------------------------------------- *)

and method_decl = { 
  m_var: var;
  m_formals: vars;
  m_throws: names;
  m_body: stmt 
}

and field = { 
  f_var: var;
  f_init: init option 
}

(* ------------------------------------------------------------------------- *)
(* Class/Interface *)
(* ------------------------------------------------------------------------- *)

and class_decl = { 
  cl_mods: modifiers;
  cl_name: ident;
  cl_super: typ option;
  cl_impls: names;
  cl_body: decls 
}

(* less: merge with class_decl? *)
and interface = { 
  if_mods: modifiers;
  if_name: ident;
  if_exts: names;
  if_body: decls 
}

(* ------------------------------------------------------------------------- *)
(* Decls *)
(* ------------------------------------------------------------------------- *)

and decl =
  | Class of class_decl
  | Interface of interface

  | Field of field
  | Method of method_decl
  | Constructor of method_decl (* the m_var.m_type should be empty *)

  | InstanceInit of stmt
  | StaticInit of stmt

and decls = decl list

(* ------------------------------------------------------------------------- *)
(* The toplevel elements *)
(* ------------------------------------------------------------------------- *)

and compilation_unit = { 
  package: name wrap option;
  imports: names;
  decls: decls;
}

and toplevel =
  | Unit of compilation_unit
  | NotParsedCorrectly of info list
  | FinalDef of info

and program = toplevel list
 (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | Expr2 of expr
  | Stmt of stmt
  | Typ of typ
  | Var of var
  | Init of init
  | Method2 of method_decl
  | Field2 of field
  | Class2 of class_decl
  | Interface2 of interface
  | Decl of decl
  | CompUnit of compilation_unit
  | Toplevel of toplevel
  | Program of program

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst

let str_of_info  = PI.str_of_info
let line_of_info = PI.line_of_info
let col_of_info  = PI.col_of_info
let pos_of_info  = PI.pos_of_info

let file_of_info = PI.file_of_info

let rewrap_str =  PI.rewrap_str

let compare_pos = PI.compare_pos

let todoii = []
let noii = []
let ast_todo = []
let ast_todo2 = ()

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
