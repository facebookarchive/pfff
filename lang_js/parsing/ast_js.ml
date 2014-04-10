(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012, 2013 Facebook
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

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* spec: http://www.ecmascript.org/ and the ecma-262 document.
 * See also http://en.wikipedia.org/wiki/ECMAScript
 *
 * See parser_js.mly top comment to see the Javascript extensions currently
 * supported.
 *
 * todo:
 *  - imitate https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API ?
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* Contains among other things the position of the token through
 * the Parse_info.token_location embedded inside it, as well as the
 * the transformation field that makes possible spatch on javascript code.
 *)
type tok = Parse_info.info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * tok

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok
and 'a angle = tok * 'a * tok
(* can now have a Right tok at the very end with trailing comma extension *)
and 'a comma_list = ('a, tok (* the comma *)) Common.either list

(* semicolon. Can be None when was implicitely inserted during parsing *)
and sc = tok option

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)
type name = string wrap
 (* with tarzan *)

type xhp_tag = string

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr =
   | L of litteral
   | V of name
   | This of tok

   (* includes new/delete/... *)
   | U of unop wrap * expr
   | B of expr * binop wrap * expr

   | Bracket of expr * expr bracket
   | Period of expr * tok (* . *) * name

   (* can have some trailing comma *)
   | Object of property comma_list brace
   (* The comma_list can have successive Left because of "elison" *)
   | Array of expr comma_list bracket

   (* Call, see also Encaps that is a sort of call when 'name' is not None  *)
   | Apply of expr * expr comma_list paren
   | Conditional of expr * tok (* ? *) * expr * tok (* : *) * expr

   (* bad language, should be in statements *)
   | Assign of expr * assignment_operator wrap * expr
   | Seq of expr * tok (* , *) * expr

   | Function of func_decl
   (* es6-ext: aka short lambdas *)
   | Arrow of arrow_func

   | Encaps of name option * tok (* ` *) * encaps list * tok (* ` *)
   | XhpHtml of xhp_html

   (* unparser: *)
   | Paren of expr paren

     and litteral =
       | Bool of bool wrap
       | Num of string wrap
       (* todo?  | Float of float | Int of int32 *)

       (* see also XhpText, EncapsString and XhpAttrString *)
       | String of string wrap
       | Regexp of string wrap (* todo? should split the flags *)
       | Null of tok

       | Undefined (* ?? *)

     and unop =
       | U_new | U_delete
       | U_void | U_typeof
       | U_bitnot
       | U_pre_increment  | U_pre_decrement
       | U_post_increment | U_post_decrement
       | U_plus | U_minus | U_not
       | U_spread

     and binop =
       | B_instanceof  | B_in

       | B_mul  | B_div  | B_mod  | B_add  | B_sub
       | B_le  | B_ge  | B_lt  | B_gt
       | B_lsr  | B_asr  | B_lsl
       | B_equal
       | B_notequal  | B_physequal  | B_physnotequal
       | B_bitand  | B_bitor  | B_bitxor
       | B_and  | B_or

     and property_name =
       | PN_String of name
       | PN_Num of string (* todo? PN_Float of float | PN_Int of int32 *) wrap

     and assignment_operator =
       | A_eq
       | A_mul  | A_div  | A_mod  | A_add  | A_sub
       | A_lsl  | A_lsr  | A_asr
       | A_and  | A_xor  | A_or

   and property =
       | P_field of property_name * tok (* : *) * expr
       | P_method of func_decl

 (* facebook-ext: JSX extension, similar to XHP for PHP *)
 and xhp_html =
   | Xhp of xhp_tag wrap * xhp_attribute list * tok (* > *) *
       xhp_body list * xhp_tag option wrap
   | XhpSingleton of xhp_tag wrap * xhp_attribute list * tok (* /> *)

   and xhp_attribute = xhp_attr_name * tok (* = *) * xhp_attr_value
    and xhp_attr_name = string wrap (* e.g. task-bar *)
    and xhp_attr_value =
      | XhpAttrString of string wrap
      | XhpAttrExpr of expr brace
   and xhp_body =
     | XhpText of string wrap
     | XhpExpr of expr option brace
     | XhpNested of xhp_html

 (* es6-ext: template string, aka interpolated/encapsulated strings *)
 and encaps =
 | EncapsString of string wrap
 (* could use 'expr brace', but it's not a regular brace for { *)
 | EncapsExpr of tok (* ${ *) * expr * tok (* } *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and st =
  | Variable of tok (* var *) * variable_declaration comma_list * sc
  | Const of tok (* const *) * variable_declaration comma_list * sc

  | Block of toplevel list brace
  | Nop of sc
  | ExprStmt of expr * sc

  | If of tok * expr paren * st * (tok (* else *) * st) option
  | Do of tok * st * tok (* while *) * expr paren * sc
  | While of tok * expr paren * st
  | For of tok * tok (* ( *) *
      lhs_or_var option * tok (* ; *) *
      expr option * tok (* ; *) *
      expr option *
      tok (* ) *) *
      st
  | ForIn of tok * tok (* ( *) * lhs_or_var * tok (* in *) *
      expr * tok (* ) *) * st
  | Switch of tok * expr paren *
      case_clause list brace (* was   (case_clause list * st) list *)

  | Continue of tok * label option * sc
  | Break of tok * label option * sc

  | Return of tok * expr option * sc

  | With of tok * expr paren * st
  | Labeled of label * tok (*:*) * st

  | Throw of tok * expr * sc
  | Try of tok * st (* always a block *) *
      (tok * arg paren * st) option * (* catch *)
      (tok * st) option (* finally *)

  and label = string wrap

  and lhs_or_var =
    | LHS of expr
    | Vars of tok * variable_declaration comma_list

  and case_clause =
    | Default of tok * tok (*:*) * toplevel list
    | Case of tok * expr * tok (*:*) * toplevel list

  and arg = string wrap

(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
(* facebook-ext: complex type annotation *)
and type_ =
  (* used for builtin types like 'void', 'number', 'string', 'any/mixed' *)
  | TName of nominal_type
  | TQuestion of tok * type_
  | TFun of param_types * tok (* => *) * type_
  (* comma_list or semicolons_list ?*)
  | TObj of (name * annotation * sc) list brace

(* Most of the time expr is a (V name),
   but Javascript allows qualified names of the form Period(e,tok,name),
   and other ways of dynamically computing types as well.
*)
and nominal_type =
  expr * type_argument comma_list angle option

and type_argument = type_

and type_parameter = name

and type_opt = annotation option

and annotation =
  | TAnnot of tok (* : *) * type_
  | TFunAnnot of type_parameters option * param_types * tok (* : *) * type_

and type_parameters = type_parameter comma_list angle

and param_types = (param_name * annotation) comma_list paren

and param_name =
  | RequiredParam of name
  | OptionalParam of name * tok (* ? *)
  | RestParam of tok (* ... *) * name

(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
and func_decl = {
  f_tok: tok option; (* None for methods *)
  f_name: name option; (* None for anonymous functions *)
  f_type_params: type_parameters option;
  f_params: parameter comma_list paren;
  f_return_type: type_opt;
  f_body: toplevel list brace;
}
  and parameter = {
   p_name: name;
   p_type: type_opt;
   (* if not None, then can be followed only by other default parameters or a
      dots parameter in a parameter comma_list *)
   p_default: default option;
   (* if not None, then should be last parameter in a parameter comma_list *)
   p_dots: tok (* ... *) option;
  }

  and default =
  | DNone of tok (* ? *)
  | DSome of tok (* = *) * expr

(* todo? could factorize with func_def, but this will require many
 * elements to be fake token, e.g. the parenthesis for parameters
 * when have only one parameter, the brace and semicolon when the body
 * is a simple expression.
 *)
and arrow_func = {
  a_params: arrow_params;
  a_return_type: type_opt;
  a_tok: tok (* => *);
  a_body: arrow_body;
 }
 and arrow_params =
 | ASingleParam of parameter
 | AParams of parameter comma_list paren
 and arrow_body =
 | AExpr of expr
 | ABody of toplevel list brace

(* ------------------------------------------------------------------------- *)
(* Variables definition *)
(* ------------------------------------------------------------------------- *)
and variable_declaration = {
  v_name: name;
  v_init: (tok (*=*) * expr) option;
  v_type: type_opt;
}

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
and class_decl = {
  c_tok: tok;
  c_name: name;
  c_type_params: type_parameter comma_list angle option;
  c_extends: (tok (* extends *) * nominal_type) option;
  c_body: class_stmt list brace;
}

  and class_stmt =
  | Method of tok option (* static *) * func_decl
  | Field of name * annotation * sc
  | ClassExtraSemiColon of sc

(* ------------------------------------------------------------------------- *)
(* Interface definition *)
(* ------------------------------------------------------------------------- *)
and interface_decl = {
  i_tok: tok;
  i_name: name;
  i_type_params: type_parameter comma_list angle option;
  i_type: type_;
}

(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
and toplevel =
  | St of st
  | FunDecl of func_decl
  | ClassDecl of class_decl
  | InterfaceDecl of interface_decl

 and program = toplevel list
 (* with tarzan *)

type any =
  | Expr of expr
  | Stmt of st
  | Func of func_decl
  | Toplevel of toplevel
  | Program of program
 (* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst
let unparen (_,x,_) = x
let unbrace = unparen
let unbracket = unparen

let uncomma xs = Common.map_filter (function
  | Left e -> Some e
  | Right _info -> None
  ) xs

let info_of_name (_s, info) = info

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the AST to add some info about the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' operator to compare Ast elements. To overcome this problem, to
 * be able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" (al) information.
 *)

let al_info x =
  { x with PI.token = PI.Ab }

(*****************************************************************************)
(* Views *)
(*****************************************************************************)

(* examples:
 * inline more static funcall in expr type or variable type
 *)

(*****************************************************************************)
(* Helpers, could also be put in lib_parsing.ml instead *)
(*****************************************************************************)

let fakeInfoAttach info =
  let info = PI.rewrap_str "FAKE" info in
  let pinfo = PI.token_location_of_info info in
  { PI.
    token = PI.FakeTokStr ("FAKE", Some (pinfo, -1));
    transfo = PI.NoTransfo;
  }

let remove_quotes_if_present s =
  (* for JX the entity names are passed as strings when
   * defining the entity (e.g. JX.Install('Typeahead'. ...)
   * but use normally (e.g. var x = JX.Typeahead(...))
   * so here we normalize.
   *)
  match s with
  | _ when s =~ "'\\(.*\\)'$" -> Common.matched1 s
  | _ when s =~ "\"\\(.*\\)\"$" -> Common.matched1 s
  | _ -> s
