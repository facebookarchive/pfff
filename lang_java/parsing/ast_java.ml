(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License 
 *)

(*****************************************************************************)
(* The AST java related types *)
(*****************************************************************************)

(* forunparser: *)

type info = { 
  pinfo : Common.parse_info;
  comments_tag: comments_around ref; (* set in comment_annotater.ml *)
  (* todo? token_info : sometimes useful to know what token it was *)
}
and il = info list

(* wrap2 is like wrap, except that I use it often for separator such
 * as ','. In that case the info is associated to the argument that
 * follows, so in 'a,b' I will have in the list [(a,[]); (b,[','])]. *)
and 'a wrap  = 'a * il
and 'a wrap2 = 'a * il






(* ------------------------------------------------------------------------- *)
(* Ident, namespace *)
(* ------------------------------------------------------------------------- *)
and ident = string wrap (* could do a wrap3 where wrap3 = just 1 info *)

and name = ident (*wrap2 '.' *) list

and names = name list

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)

and typ = typbis wrap
 and typbis = 
  | TypeName of name (* include the 'void', 'int', and other primitive type *)
  | ArrayType of typ


(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)

and expr = exprbis wrap
  and exprbis = 
  | Name of name (* include 'this' and 'super' special names *)
  | Literal of string
  | ClassLiteral of typ

  | NewClass of typ * exprs * decls wrap (* { } *) option
  | NewQualifiedClass of expr * ident * exprs * decls wrap (* { } *) option
  | NewArray of typ * exprs * int * init option

  | Dot of expr * ident
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
(* Statement *)
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
  | For of stmts * expr option * stmts * stmt

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

and catch = var * stmt

and cases = case list
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

and modifiers = modifier list

and vars = var list

and var =
  { v_mods : modifiers;
    v_type : typ;
    v_name : ident }


and init = initbis wrap
  and initbis =
  | ExprInit of expr
  | ArrayInit of init list


(* ------------------------------------------------------------------------- *)
(* Method, field *)
(* ------------------------------------------------------------------------- *)

and method_decl =
  { m_var : var;
    m_formals : vars;
    m_throws : names;
    m_body : stmt }

and field =
  { f_var : var;
    f_init : init option }


(* ------------------------------------------------------------------------- *)
(* Class *)
(* ------------------------------------------------------------------------- *)

and class_decl =
  { cl_mods : modifiers;
    cl_name : ident;
    cl_super : name option;
    cl_impls : names;
    cl_body : decls }

and interface =
  { if_mods : modifiers;
    if_name : ident;
    if_exts : names;
    if_body : decls }



(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
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


and compilation_unit =
  { package : name wrap option;
    imports : names;
    decls : decls;
    (* pad: comments : Source.comments *)
  }

and program = (compilation_unit, info list) Common.either

and toplevel = compilation_unit

(* ------------------------------------------------------------------------- *)



(*****************************************************************************)
(* C comments *)
(*****************************************************************************)

(* I often use m for comments as I can not use c (already use for c stuff) 
 * and com is too long.
 *)

(* this type will be associated to each token *)
and comments_around = {
  mbefore: comment_and_relative_pos list;
  mafter:  comment_and_relative_pos list;
}
  and comment_and_relative_pos = {

   minfo: Common.parse_info;
   (* the int represent the number of lines of difference between the
    * current token and the comment. When on same line, this number is 0.
    * When previous line, -1. In some way the after/before in previous
    * record is useless because the sign of the integer can helps
    * do the difference too, but I keep it that way.
    *)
   mpos: int;
   (* todo?
    *  is_alone_in_line: bool; (*for labels, to avoid false positive*)
    *)
 }

and comment = Common.parse_info
and com = comment list ref



let emptyComments= {
  mbefore = [];
  mafter = [];
}

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst

let pos_of_info   ii = ii.pinfo.Common.charpos
let str_of_info ii = ii.pinfo.Common.str 
let file_of_info ii = ii.pinfo.Common.file
let line_of_info ii = ii.pinfo.Common.line
let col_of_info ii = ii.pinfo.Common.column

let parse_info_of_info ii = ii.pinfo


let rewrap_str s ii =  
  let oldpinfo = ii.pinfo in
  { ii with pinfo = 
      { oldpinfo with 
        Common.str = s 
      }
  }

let todoii = []
let noii = []



let compare_pos ii1 ii2 =
    compare ii1.pinfo.Common.charpos ii2.pinfo.Common.charpos

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
