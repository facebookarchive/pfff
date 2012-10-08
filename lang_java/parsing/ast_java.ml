(* Joust: a Java lexer, parser, and pretty-printer written in OCaml.
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License 
 * 
 * Yoann Padioleau: 
 * 2010, port to the pfff infrastructure.
 * 2012, support annotations, generics, enum, foreach, etc
 *)

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A simple AST for Java.
 * 
 * For Java we directly do a simple AST, as opposed to a CST (Concrete
 * Syntax Tree) as in lang_php/, which should be enough for higlight_java.ml
 * I think. We just need the full list of tokens + the AST with position
 * for the identifiers.
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
type info = Parse_info.info
type 'a wrap  = 'a * info

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
(* for class/interface/enum names, method/field names, type parameter, ... *)
type ident = string wrap

(* for package, import, throw specification *)
type qualified_ident = ident list

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

type typ =
  (* 'void', 'int', and other primitive types *)
  | TBasic of string wrap
  (* not sure why they call that a reference type *)
  | TRef of ref_type
  (* less: seems to have been removed in recent java grammar *)
  | ArrayType of typ

 and ref_type = 
   (ident * type_argument list) list1
  and type_argument =
    | TArgument of ref_type
    | TQuestion of (bool (* extends|super, true = super *) * ref_type) option 
  and 'a list1 = 'a list (* really should be 'a * 'a list *)

type type_parameter =
  | TParam of ident * ref_type list (* extends *)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* when do we need to have a name using type arguments?
 * for certain calls like List.<Int>of(), which is rare.
 * Do a NameGeneric instead ? the type_argument can be
 * only at the end?
 *)
type name = (type_argument list * ident) list1

(* Can have nested anon class (=~ closures) in expressions hence
 * the use of type ... and ... below
 *)
type expr =
  (* include 'this' and 'super' special names *)
  | Name of name 

  (* todo: split in constant type with Int | Float | String | Char | Bool *)
  | Literal of string wrap

  (* Xxx.class *)
  | ClassLiteral of typ

  | NewClass of typ * exprs * decls option
  (* ?? *)
  | NewQualifiedClass of expr * ident * exprs * decls option
  | NewArray of typ * exprs * int * init option

  | Call of expr * exprs
  | Dot of expr * ident
  | ArrayAccess of expr * expr

  | Postfix of expr * op
  | Prefix of op * expr
  | Infix of expr * op * expr

  | Cast of typ * expr

  | InstanceOf of expr * ref_type

  | Conditional of expr * expr * expr
  (* ugly java, as C, assignement is an expression not a statement :( *)
  | Assignment of expr * op * expr

and exprs = expr list

and op = string

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)

and stmt =
  | Empty
  | Block of stmts
  | Expr of expr

  | If of expr * stmt * stmt
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

and case =
  | Case of expr
  | Default
and cases = case list

(* TODO *)
and for_control = unit

and catch = var * stmt
and catches = catch list

(* ------------------------------------------------------------------------- *)
(* variable (local var, parameter) declaration *)
(* ------------------------------------------------------------------------- *)

and var = { 
  v_name: ident;
  v_mods: modifiers;
  v_type: typ;
}

and modifier =
  | Public   | Protected   | Private
  | Abstract
  | Static 
  | Final
  | StrictFP
  | Transient   | Volatile
  | Synchronized
  | Native

  (* TODO *)
  | Annotation

 and modifiers = modifier wrap list

and vars = var list

(* ------------------------------------------------------------------------- *)
(* Method, field *)
(* ------------------------------------------------------------------------- *)

(* method or constructor *)
and method_decl = { 
  (* v_typ is a (TBasic void) for a constructor *)
  m_var: var;
  (* the v_mod can only be Final or Annotation *)
  m_formals: vars;
  m_throws: qualified_ident list;
  (* Empty for methods in interfaces. 
   * For constructor the first stmts can contain 
   * explicit_constructor_invocations.
   *)
  m_body: stmt 
}

and field = { 
  f_var: var;
  f_init: init option 
}

(* less: could merge with expr *)
and init =
  | ExprInit of expr
  | ArrayInit of init list

(* ------------------------------------------------------------------------- *)
(* Class/Interface *)
(* ------------------------------------------------------------------------- *)

and class_decl = { 
  cl_name: ident;
  cl_kind: class_kind;

  cl_tparams: type_parameter list;

  cl_mods: modifiers;

  (* always at None for interface *)
  cl_extends: typ option;
  (* for interface this is actually the extends *)
  cl_impls: ref_type list;

  (* the methods body are always empty for interface *)
  cl_body: decls 
}
  and class_kind = ClassRegular | Interface

(* ------------------------------------------------------------------------- *)
(* Decls *)
(* ------------------------------------------------------------------------- *)

and decl =
  | Class of class_decl
  | Method of method_decl
  | Field of field
  | Init of bool (* static *) * stmt

and decls = decl list

(* ------------------------------------------------------------------------- *)
(* The toplevel elements *)
(* ------------------------------------------------------------------------- *)

type compilation_unit = { 
  package: qualified_ident option;
  (* The qualified ident can also contain "*" at the very end.
   * The bool is for static import (javaext:)
   *)
  imports: (bool * qualified_ident) list;
  (* todo? necessarily a Class first? *)
  decls: decls;
}

type program = compilation_unit

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | Expr2 of expr
  | Stmt of stmt
  | Typ of typ
  | Var of var
  | Init2 of init
  | Method2 of method_decl
  | Field2 of field
  | Class2 of class_decl
  | Decl of decl
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

let fakeInfo ?(next_to=None) str = { PI.
  token = PI.FakeTokStr (str, next_to);
  comments = ();
  transfo = PI.NoTransfo;
}

let ast_todo = []
let ast_todo2 = ()

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
