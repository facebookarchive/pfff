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
 *  - try to imitate a bit:
 *    https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
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
(* with trailing comma extension, can have a Right tok at the very end *)
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

   | Object of field comma_list brace
   (* The comma_list can have successive Left because of "elison" *)
   | Array of expr comma_list bracket

   | Apply of expr * expr comma_list paren
   | Conditional of expr * tok (* ? *) * expr * tok (* : *) * expr

   (* bad language, should be in statements *)
   | Assign of expr * assignment_operator wrap * expr
   | Seq of expr * tok (* , *) * expr

   | Function of func_decl
   (* aka short lambdas *)
   | Arrow of arrow_func

   (* unparser: *)
   | Extra of extra
   | Paren of expr paren

   | XhpHtml of xhp_html

     and extra = 
       (* ??? *)
       | DanglingComma

     and litteral =
       | Bool of bool wrap
       | Num of string wrap
       (* todo?  | Float of float | Int of int32 *)

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
       | PN_Empty (* ?? *)
           
     and assignment_operator =
       | A_eq
       | A_mul  | A_div  | A_mod  | A_add  | A_sub
       | A_lsl  | A_lsr  | A_asr
       | A_and  | A_xor  | A_or
             
   and field =
      (property_name * tok (* : *) * expr)

 (* facebook-ext: JSX extension *)
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
     | XhpExpr of expr brace
     | XhpNested of xhp_html

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
  | TName of name
  | TQuestion of tok * type_
  | TArray of tok * type_ angle
  | TFun of type_ comma_list paren * tok (* => *) * type_
  (* comma_list or semicolons_list ?*)
  | TObj of (name * tok (* : *) * type_) comma_list brace

and type_opt = (tok (* : *) * type_) option

(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
and func_decl = {
  f_tok: tok option; (* None for methods *)
  f_name: name option; (* None for anonymous functions *)
  f_params: parameter comma_list paren;
  f_return_type: type_opt;
  f_body: toplevel list brace;
}
  and parameter = {
   p_name: name;
   p_type: type_opt;
  }

(* todo? could factorize with func_def, but this will require many
 * elements to be fake token, e.g. the parenthesis for parameters
 * when have only one parameter, the brace and semicolon when the body
 * is a simple expression.
 *)
and arrow_func = {
  a_params: arrow_params;
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
  c_extends: (tok (* extends *) * inherit_expr) option;
  c_body: class_stmt list brace;
}

  and class_stmt =
  | Method of tok option (* static *) * func_decl
  | Field of parameter * sc
  | ClassExtraSemiColon of sc

and inherit_expr = expr

(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
and toplevel =
  | St of st
  | FunDecl of func_decl
  | ClassDecl of class_decl

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
