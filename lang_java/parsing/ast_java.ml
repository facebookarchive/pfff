(* Joust: a Java lexer, parser, and pretty-printer written in OCaml.
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License
 *
 * Yoann Padioleau:
 * 2010 port to the pfff infrastructure.
 * 2012 heavily modified to support annotations, generics, enum, foreach, etc
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A simple AST for Java.
 *
 * For Java we directly do a simple AST, as opposed to a CST (Concrete
 * Syntax Tree) as in lang_php/. This should be enough for higlight_java.ml
 * I think (we just need the full list of tokens + the AST with position
 * for the identifiers).
 *
 * todo:
 *  - support generic methods (there is support for generic classes though)
 *)

(*****************************************************************************)
(* The AST java related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Parse_info.info
and 'a wrap  = 'a * tok
  (* with tarzan *)

type 'a list1 = 'a list (* really should be 'a * 'a list *)

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
(* for class/interface/enum names, method/field names, type parameter, ... *)
type ident = string wrap
 (* with tarzan *)

(* for package, import, throw specification *)
type qualified_ident = ident list
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

type typ =
  (* 'void', 'int', and other primitive types; could be merged with TClass *)
  | TBasic of string wrap
  | TClass of class_type
  | TArray of typ

  (* class or interface or enum type actually *)
 and class_type =
   (ident * type_argument list) list1

  and type_argument =
    | TArgument of ref_type
    | TQuestion of (bool (* extends|super, true = super *) * ref_type) option

   (* A ref type should be a class type or an array of whatever, but not a
    * primitive type. We don't enforce this invariant in the AST to simplify
    * things.
    *)
    and ref_type = typ
    (* with tarzan *)

type type_parameter =
  | TParam of ident * ref_type list (* extends *)
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Modifiers *)
(* ------------------------------------------------------------------------- *)

type modifier =
  | Public | Protected | Private
  | Abstract
  | Static
  | Final
  | StrictFP
  | Transient | Volatile
  | Synchronized
  | Native

  | Annotation of annotation

 and annotation = name_or_class_type * (annotation_element option)

 and modifiers = modifier wrap list

 and annotation_element =
   | AnnotArgValue of element_value
   | AnnotArgPairInit of annotation_pair list
   | EmptyAnnotArg
 and element_value =
   | AnnotExprInit of expr
   | AnnotNestedAnnot of annotation
   | AnnotArrayInit of element_value list

 and annotation_pair = (ident * element_value)

 and name_or_class_type = identifier_ list
 and identifier_ =
   | Id of ident
   | Id_then_TypeArgs of ident * type_argument list
   | TypeArgs_then_Id of type_argument list * identifier_

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* When do we need to have a name with actual type_argument?
 * For certain calls like List.<Int>of(), which are rare.
 * less: do a NameGeneric instead? the type_argument could then be
 *  only at the end?
 *)
and name = (type_argument list * ident) list1

(* Can have nested anon class (=~ closures) in expressions hence
 * the use of type ... and ... below
 *)
and expr =
  (* Name is used for local variable, 'this' and 'super' special names,
   * and statically computable entities such as Package1.subpackage.Class.
   * Field or method accesses should use Dot (see below). Unfortunately
   * the Java grammar is ambiguous and without contextual information,
   * there is no way to know whether x.y.z is an access to the field z
   * of field y of local variable x or the static field z of class y
   * in package x. See the note on Dot below.
   *)
  | Name of name

 (* This is used only in the context of annotations *)
  | NameOrClassType of name_or_class_type

  (* less: split in constant type with Int | Float | String | Char | Bool *)
  | Literal of string wrap

  (* Xxx.class *)
  | ClassLiteral of typ

  (* the 'decls option' is for anon classes *)
  | NewClass of typ * arguments * decls option
  | NewArray of typ * arguments * int * init option
  (* see tests/java/parsing/NewQualified.java *)
  | NewQualifiedClass of expr * ident * arguments * decls option

  | Call of expr * arguments

  (* How is parsed X.y ? Could be a Name [X;y] or Dot (Name [X], y)?
   * The static part should be a Name and the more dynamic part a Dot.
   * So variable.field and variable.method should be parsed as
   * Dot (Name [variable], field|method). Unfortunately
   * variable.field1.field2 is currently parsed as
   * Dot (Name [variable;field1], field2). You need semantic information
   * about variable to disambiguate.
   *
   * Why the ambiguity? Names and packages are not
   * first class citizens, so one cant pass a class/package name as an
   * argument to a function, so when have X.Y.z in an expression, the
   * last element has to be a field or a method (if it's a class,
   * people should use X.Y.class), so it's safe to transform such
   * a Name at parsing time in a Dot.
   * The problem is that more things in x.y.z can be a Dot, but to know
   * that requires semantic information about the type of x and y.
   *)
  | Dot of expr * ident

  | ArrayAccess of expr * expr

  | Postfix of expr * op
  | Prefix of op * expr
  | Infix of expr * op * expr

  | Cast of typ * expr

  | InstanceOf of expr * ref_type

  | Conditional of expr * expr * expr
  (* ugly java, like in C assignement is an expression not a statement :( *)
  | Assignment of expr * op * expr

and arguments = expr list

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
  | LocalVar of var_with_init

  | LocalClass of class_decl

  (* javaext: http://java.sun.com/j2se/1.4.2/docs/guide/lang/assert.html *)
  | Assert of expr * expr option (* assert e or assert e : e2 *)

and stmts = stmt list

and case =
  | Case of expr
  | Default
and cases = case list

and for_control =
  | ForClassic of for_init * expr list * expr list
  | Foreach of var * expr
  and for_init =
    | ForInitVars of var_with_init list
    | ForInitExprs of expr list

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

and vars = var list

(* less: could be merged with var *)
and var_with_init = {
  f_var: var;
  f_init: init option
}

  (* less: could merge with expr *)
  and init =
    | ExprInit of expr
    | ArrayInit of init list

(* ------------------------------------------------------------------------- *)
(* Methods, fields *)
(* ------------------------------------------------------------------------- *)

(* method or constructor *)
and method_decl = {
  (* m_var.v_type is a (TBasic void) for a constructor *)
  m_var: var;
  (* the var.v_mod in params can only be Final or Annotation *)
  m_formals: vars;
  m_throws: qualified_ident list;

  (* todo: m_tparams *)

  (* Empty for methods in interfaces.
   * For constructor the first stmts can contain
   * explicit_constructor_invocations.
   *)
  m_body: stmt
}

and field = var_with_init

(* ------------------------------------------------------------------------- *)
(* Enum *)
(* ------------------------------------------------------------------------- *)

and enum_decl = {
  en_name: ident;
  en_mods: modifiers;
  en_impls: ref_type list;
  en_body: enum_constant list * decls;
}
 and enum_constant =
   | EnumSimple of ident
   (* http://docs.oracle.com/javase/1.5.0/docs/guide/language/enums.html *)
   | EnumConstructor of ident * arguments
   | EnumWithMethods of ident * method_decl list

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
  | Enum of enum_decl
  | Init of bool (* static *) * stmt

and decls = decl list

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* The toplevel elements *)
(* ------------------------------------------------------------------------- *)

type compilation_unit = {
  package: qualified_ident option;
  (* The qualified ident can also contain "*" at the very end.
   * The bool is for static import (javaext:)
   *)
  imports: (bool * qualified_ident) list;
  (* todo? necessarily a (unique) class first? *)
  decls: decls;
}
 (* with tarzan *)

type program = compilation_unit
 (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | AIdent of ident
  | AExpr of expr
  | AStmt of stmt
  | ATyp of typ
  | AVar of var
  | AInit of init
  | AMethod of method_decl
  | AField of field
  | AClass of class_decl
  | ADecl of decl
  | AProgram of program
 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let unwrap = fst

let fakeInfo ?(next_to=None) str = { Parse_info.
  token = Parse_info.FakeTokStr (str, next_to);
  transfo = Parse_info.NoTransfo;
}

let ast_todo = []
let ast_todo2 = ()

let info_of_ident ident =
  snd ident

let is_final xs =
  let xs = List.map fst xs in
  List.mem Final xs
let is_final_static xs =
  let xs = List.map fst xs in
  List.mem Final xs && List.mem Static xs

let rec info_of_identifier_ (id : identifier_) : tok = match id with
  | Id id
  | Id_then_TypeArgs (id, _) -> snd id
  | TypeArgs_then_Id (_, id_) -> info_of_identifier_ id_
