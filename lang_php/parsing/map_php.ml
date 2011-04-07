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

open Ocaml

open Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

module Type_php = struct
open Type_php
let rec map_phptype v = map_of_list map_phptypebis v
and map_phptypebis =
  function
  | Basic v1 -> let v1 = map_basictype v1 in Basic ((v1))
  | ArrayFamily v1 -> let v1 = map_arraytype v1 in ArrayFamily ((v1))
  | Object vs -> let v1 = map_of_list map_of_string vs in Object ((v1))
  | Function ((v1, v2)) ->
      let v1 = map_of_list (map_of_option map_phptype) v1 in
      let v2 = map_phptype v2 in
      Function ((v1, v2))
  | Null -> Null
  | Resource -> Resource
  | TypeVar v1 -> let v1 = map_of_string v1 in TypeVar ((v1))
  | Unknown -> Unknown
  | Top -> Top
and map_basictype =
  function
  | Bool -> Bool
  | Int -> Int
  | Float -> Float
  | String -> String
  | Unit -> Unit
and map_arraytype =
  function
  | Array v1 -> let v1 = map_phptype v1 in Array ((v1))
  | Hash v1 -> let v1 = map_phptype v1 in Hash ((v1))
  | Record v1 ->
      let v1 =
        map_of_list
          (fun (v1, v2) ->
             let v1 = map_of_string v1 and v2 = map_phptype v2 in (v1, v2))
          v1
      in Record ((v1))
  
end



(* hooks *)
type visitor_in = {
  kexpr: (expr  -> expr) * visitor_out -> expr  -> expr;
  klvalue: (lvalue  -> lvalue) * visitor_out -> lvalue  -> lvalue;
  kstmt_and_def: 
    (stmt_and_def -> stmt_and_def) * visitor_out -> stmt_and_def ->stmt_and_def;
  kstmt: (stmt -> stmt) * visitor_out -> stmt -> stmt;
  kqualifier: (qualifier -> qualifier) * visitor_out -> qualifier -> qualifier;
  kclass_def:  (class_def -> class_def) * visitor_out -> class_def -> class_def;

  kinfo: (info -> info) * visitor_out -> info -> info;
                                                                            
}
and visitor_out = {
  vtop: toplevel -> toplevel;
  vstmt_and_def: stmt_and_def -> stmt_and_def;
  vprogram: program -> program;
  vexpr: expr -> expr;
  vlvalue: lvalue -> lvalue;
  vxhpattrvalue: xhp_attr_value -> xhp_attr_value;
  vany: any -> any;
}

let default_visitor = 
  { kexpr   = (fun (k,_) x -> k x);
    klvalue   = (fun (k,_) x -> k x);
    kstmt_and_def = (fun (k,_) x -> k x);
    kstmt = (fun (k,_) x -> k x);
    kqualifier = (fun (k,_) x -> k x);
    kclass_def = (fun (k,_) x -> k x);
    kinfo = (fun (k,_) x -> k x);
  }

let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

(* start of auto generation *)


  
let rec map_info x = 
  let rec k x = 
    match x with
    { Parse_info.token = v_pinfo; 
      transfo = v_transfo;
      comments = v_comments;
    } ->
    let v_pinfo = 
      (* todo? map_pinfo v_pinfo *)
    v_pinfo
    in
    (* not recurse in transfo ? *)
    { Parse_info.token = v_pinfo;   (* generete a fresh field *)
      transfo = v_transfo;
      comments = v_comments;
    }
  in
  vin.kinfo (k, all_functions) x

and map_tok v = map_info v
and map_wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap_string _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap_tag _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap_modif _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap_xhp_tag _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap2 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap3 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap4 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap5 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap6 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap7 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)

and map_wrap10 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)
and map_wrap11 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = map_info v2 in (v1, v2)

and map_paren_args _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren_expr _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren_params _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren_lexical_vars _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren2 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren3 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren4 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren5 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren6 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren8 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren9 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren10 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren11 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren12 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren13 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_paren16 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)


and map_brace_body _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_brace_expr _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)
and map_brace_class_body _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)

and map_brace_1 _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)

and map_bracket _of_a (v1, v2, v3) =
  let v1 = map_tok v1 and v2 = _of_a v2 and v3 = map_tok v3 in (v1, v2, v3)

and map_comma_list_arg _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list_var _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list_expr _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list_param _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list2 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list3 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list4 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list6 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list7 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list8 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list10 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list12 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list13 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list14 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
and map_comma_list15 _of_a xs = 
  map_of_list (fun x -> Ocaml.map_of_either _of_a map_info x) xs
  
and map_name =
  function
  | Name v1 -> let v1 = map_wrap_string map_of_string v1 in Name ((v1))
  | XhpName v1 -> let v1 = map_wrap_tag (map_of_list map_of_string) v1 in 
                  XhpName ((v1))
and map_xhp_tag v = map_of_list map_of_string v
and map_dname =
  function | DName v1 -> let v1 = map_wrap_string map_of_string v1 in DName ((v1))
and map_qualifier x =
  let rec k x =
    match x with
  | Qualifier ((v1, v2)) ->
      let v1 = map_fully_qualified_class_name v1
      and v2 = map_tok v2
      in Qualifier ((v1, v2))
  | Self ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in Self ((v1, v2))
  | Parent ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in Parent ((v1, v2))
  in
  vin.kqualifier (k, all_functions) x

and map_fully_qualified_class_name v = map_name v

  
and map_ptype =
  function
  | BoolTy -> BoolTy
  | IntTy -> IntTy
  | DoubleTy -> DoubleTy
  | StringTy -> StringTy
  | ArrayTy -> ArrayTy
  | ObjectTy -> ObjectTy
  
and map_expr (x) =
  let k x =  match x with (v1, v2) ->

 let map_exp_info { t = v_t } =
  let v_t = Type_php.map_phptype v_t in { t = v_t }
 in
 let map_exprbis =
  function
  | Lv v1 -> let v1 = map_variable v1 in Lv ((v1))
  | Sc v1 -> let v1 = map_scalar v1 in Sc ((v1))
  | Binary ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_wrap2 map_binaryOp v2
      and v3 = map_expr v3
      in Binary ((v1, v2, v3))
  | Unary ((v1, v2)) ->
      let v1 = map_wrap3 map_unaryOp v1
      and v2 = map_expr v2
      in Unary ((v1, v2))
  | Assign ((v1, v2, v3)) ->
      let v1 = map_variable v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      in Assign ((v1, v2, v3))
  | AssignOp ((v1, v2, v3)) ->
      let v1 = map_variable v1
      and v2 = map_wrap4 map_assignOp v2
      and v3 = map_expr v3
      in AssignOp ((v1, v2, v3))
  | Postfix ((v1, v2)) ->
      let v1 = map_rw_variable v1
      and v2 = map_wrap5 map_fixOp v2
      in Postfix ((v1, v2))
  | Infix ((v1, v2)) ->
      let v1 = map_wrap6 map_fixOp v1
      and v2 = map_rw_variable v2
      in Infix ((v1, v2))
  | CondExpr ((v1, v2, v3, v4, v5)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      and v4 = map_tok v4
      and v5 = map_expr v5
      in CondExpr ((v1, v2, v3, v4, v5))
  | AssignList ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_paren2 (map_comma_list2 map_list_assign) v2
      and v3 = map_tok v3
      and v4 = map_expr v4
      in AssignList ((v1, v2, v3, v4))
  | ConsArray ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren3 (map_comma_list3 map_array_pair) v2
      in ConsArray ((v1, v2))
  | New ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_class_name_reference v2
      and v3 = map_of_option (map_paren_args (map_comma_list_arg map_argument)) v3
      in New ((v1, v2, v3))
  | Clone ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Clone ((v1, v2))
  | AssignRef ((v1, v2, v3, v4)) ->
      let v1 = map_variable v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      and v4 = map_variable v4
      in AssignRef ((v1, v2, v3, v4))
  | AssignNew ((v1, v2, v3, v4, v5, v6)) ->
      let v1 = map_variable v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      and v4 = map_tok v4
      and v5 = map_class_name_reference v5
      and v6 = map_of_option (map_paren_args (map_comma_list_arg map_argument)) v6
      in AssignNew ((v1, v2, v3, v4, v5, v6))
  | Cast ((v1, v2)) ->
      let v1 = map_wrap7 map_castOp v1 and v2 = map_expr v2 in Cast ((v1, v2))
  | CastUnset ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in CastUnset ((v1, v2))
  | InstanceOf ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_class_name_reference v3
      in InstanceOf ((v1, v2, v3))
  | Eval ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_paren_expr map_expr v2 in Eval ((v1, v2))
  | Lambda v1 -> let v1 = map_lambda_def v1 in Lambda ((v1))
  | Exit ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_of_option (map_paren4 (map_of_option map_expr)) v2
      in Exit ((v1, v2))
  | At ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in At ((v1, v2))
  | Print ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Print ((v1, v2))
  | BackQuote ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in BackQuote ((v1, v2, v3))
  | Include ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Include ((v1, v2))
  | IncludeOnce ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in IncludeOnce ((v1, v2))
  | Require ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Require ((v1, v2))
  | RequireOnce ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in RequireOnce ((v1, v2))
  | Yield ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_expr v2 in Yield ((v1, v2))
  | Empty ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren5 map_variable v2
      in Empty ((v1, v2))
  | Isset ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren6 (map_comma_list_var map_variable) v2
      in Isset ((v1, v2))
  | SgrepExprDots v1 -> let v1 = map_info v1 in SgrepExprDots ((v1))
  | ParenExpr v1 -> let v1 = map_paren_expr map_expr v1 in ParenExpr ((v1))
  | XhpHtml v1 -> let v1 = map_xhp_html v1 in XhpHtml ((v1))
 in
 let v1 = map_exprbis v1 and v2 = map_exp_info v2  in
 (v1, v2)
 in
 vin.kexpr (k, all_functions) x

and map_scalar =
  function
  | C v1 -> let v1 = map_constant v1 in C ((v1))
  | ClassConstant v1 ->
      let v1 =
        (match v1 with
         | (v1, v2) ->
             let v1 = map_qualifier v1 and v2 = map_name v2 in (v1, v2))
      in ClassConstant ((v1))
  | Guil ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in Guil ((v1, v2, v3))
  | HereDoc ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in HereDoc ((v1, v2, v3))
and map_constant =
  function
  | Int v1 -> let v1 = map_wrap_string map_of_string v1 in Int ((v1))
  | Double v1 -> let v1 = map_wrap_string map_of_string v1 in Double ((v1))
  | String v1 -> let v1 = map_wrap_string map_of_string v1 in String ((v1))
  | CName v1 -> let v1 = map_name v1 in CName ((v1))
  | PreProcess v1 ->
      let v1 = map_wrap map_cpp_directive v1 in PreProcess ((v1))
  | XdebugClass ((v1, v2)) ->
      let v1 = map_name v1
      and v2 = map_of_list map_class_stmt v2
      in XdebugClass ((v1, v2))
  | XdebugResource -> XdebugResource

and map_cpp_directive =
  function
  | Line -> Line
  | File -> File
  | ClassC -> ClassC
  | MethodC -> MethodC
  | FunctionC -> FunctionC
and map_encaps =
  function
  | EncapsString v1 ->
      let v1 = map_wrap_string map_of_string v1 in EncapsString ((v1))
  | EncapsVar v1 -> let v1 = map_variable v1 in EncapsVar ((v1))
  | EncapsCurly ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_variable v2
      and v3 = map_tok v3
      in EncapsCurly ((v1, v2, v3))
  | EncapsDollarCurly ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_variable v2
      and v3 = map_tok v3
      in EncapsDollarCurly ((v1, v2, v3))
  | EncapsExpr ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_expr v2
      and v3 = map_tok v3
      in EncapsExpr ((v1, v2, v3))
and map_fixOp = function | Dec -> Dec | Inc -> Inc
and map_binaryOp =
  function
  | Arith v1 -> let v1 = map_arithOp v1 in Arith ((v1))
  | Logical v1 -> let v1 = map_logicalOp v1 in Logical ((v1))
  | BinaryConcat -> BinaryConcat
and map_arithOp =
  function
  | Plus -> Plus
  | Minus -> Minus
  | Mul -> Mul
  | Div -> Div
  | Mod -> Mod
  | DecLeft -> DecLeft
  | DecRight -> DecRight
  | And -> And
  | Or -> Or
  | Xor -> Xor
and map_logicalOp =
  function
  | Inf -> Inf
  | Sup -> Sup
  | InfEq -> InfEq
  | SupEq -> SupEq
  | Eq -> Eq
  | NotEq -> NotEq
  | Identical -> Identical
  | NotIdentical -> NotIdentical
  | AndLog -> AndLog
  | OrLog -> OrLog
  | XorLog -> XorLog
  | AndBool -> AndBool
  | OrBool -> OrBool
and map_assignOp =
  function
  | AssignOpArith v1 -> let v1 = map_arithOp v1 in AssignOpArith ((v1))
  | AssignConcat -> AssignConcat
and map_unaryOp =
  function
  | UnPlus -> UnPlus
  | UnMinus -> UnMinus
  | UnBang -> UnBang
  | UnTilde -> UnTilde
and map_castOp v = map_ptype v
and map_list_assign =
  function
  | ListVar v1 -> let v1 = map_variable v1 in ListVar ((v1))
  | ListList ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren16 (map_comma_list4 map_list_assign) v2
      in ListList ((v1, v2))
  | ListEmpty -> ListEmpty
and map_array_pair =
  function
  | ArrayExpr v1 -> let v1 = map_expr v1 in ArrayExpr ((v1))
  | ArrayRef ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_variable v2 in ArrayRef ((v1, v2))
  | ArrayArrowExpr ((v1, v2, v3)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      in ArrayArrowExpr ((v1, v2, v3))
  | ArrayArrowRef ((v1, v2, v3, v4)) ->
      let v1 = map_expr v1
      and v2 = map_tok v2
      and v3 = map_tok v3
      and v4 = map_variable v4
      in ArrayArrowRef ((v1, v2, v3, v4))
and map_class_name_reference =
  function
  | ClassNameRefStatic v1 ->
      let v1 = map_fully_qualified_class_name v1 in ClassNameRefStatic ((v1))
  | ClassNameRefDynamic v1 ->
      let v1 =
        (match v1 with
         | (v1, v2) ->
             let v1 = map_variable v1
             and v2 = map_of_list map_obj_prop_access v2
             in (v1, v2))
      in ClassNameRefDynamic ((v1))
and map_obj_prop_access (v1, v2) =
  let v1 = map_tok v1 and v2 = map_obj_property v2 in (v1, v2)

and map_xhp_html =
  function
  | Xhp ((v1, v2, v3, v4, v5)) ->
      let v1 = map_wrap10 map_xhp_tag v1
      and v2 = map_of_list map_xhp_attribute v2
      and v3 = map_tok v3
      and v4 = map_of_list map_xhp_body v4
      and v5 = map_wrap11 (map_of_option map_xhp_tag) v5
      in Xhp ((v1, v2, v3, v4, v5))
  | XhpSingleton ((v1, v2, v3)) ->
      let v1 = map_wrap10 map_xhp_tag v1
      and v2 = map_of_list map_xhp_attribute v2
      and v3 = map_tok v3
      in XhpSingleton ((v1, v2, v3))
and map_xhp_attribute (v1, v2, v3) =
  let v1 = map_xhp_attr_name v1
  and v2 = map_tok v2
  and v3 = map_xhp_attr_value v3
  in (v1, v2, v3)
and map_xhp_attr_name v = map_wrap_string map_of_string v
and map_xhp_attr_value =
  function
  | XhpAttrString ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_encaps v2
      and v3 = map_tok v3
      in XhpAttrString ((v1, v2, v3))
  | XhpAttrExpr v1 -> let v1 = map_brace_expr map_expr v1 in XhpAttrExpr ((v1))
  | SgrepXhpAttrValueMvar v1 ->
      let v1 = map_wrap_string map_of_string v1 in SgrepXhpAttrValueMvar ((v1))
and map_xhp_body =
  function
  | XhpText v1 -> let v1 = map_wrap_string map_of_string v1 in XhpText ((v1))
  | XhpExpr v1 -> let v1 = map_brace_expr map_expr v1 in XhpExpr ((v1))
  | XhpNested v1 -> let v1 = map_xhp_html v1 in XhpNested ((v1))


and map_variable (v1, v2) =
  let k x = match v1, v2 with
    | ((v1, v2)) ->
        
        let v1 = map_variablebis v1 and v2 = map_var_info v2 in 
        (v1, v2)
  in
 vin.klvalue (k, all_functions) (v1,v2)

and map_lvalue a = map_variable a

and map_var_info { tlval = v_tvar } =
  let v_tvar = Type_php.map_phptype v_tvar in { tlval = v_tvar }
and map_variablebis =
  function
  | Var ((v1, v2)) ->
      let v1 = map_dname v1
      and v2 = map_of_ref Scope_code.map_scope v2
      in Var ((v1, v2))
  | This v1 -> let v1 = map_tok v1 in This ((v1))
  | VArrayAccess ((v1, v2)) ->
      let v1 = map_variable v1
      and v2 = map_bracket (map_of_option map_expr) v2
      in VArrayAccess ((v1, v2))
  | VArrayAccessXhp ((v1, v2)) ->
      let v1 = map_expr v1
      and v2 = map_bracket (map_of_option map_expr) v2
      in VArrayAccessXhp ((v1, v2))
  | VBrace ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_brace_expr map_expr v2 in 
      VBrace ((v1, v2))
  | VBraceAccess ((v1, v2)) ->
      let v1 = map_variable v1
      and v2 = map_brace_expr map_expr v2
      in VBraceAccess ((v1, v2))
  | Indirect ((v1, v2)) ->
      let v1 = map_variable v1
      and v2 = map_indirect v2
      in Indirect ((v1, v2))
  | VQualifier ((v1, v2)) ->
      let v1 = map_qualifier v1
      and v2 = map_variable v2
      in VQualifier ((v1, v2))
  | ClassVar ((v1, v2)) ->
      let v1 = map_qualifier v1
      and v2 = map_dname v2
      in ClassVar ((v1, v2))
  | FunCallSimple ((v2, v3)) ->
      let v2 = map_name v2
      and v3 = map_paren_args (map_comma_list_arg map_argument) v3
      in FunCallSimple ((v2, v3))
  | FunCallVar ((v1, v2, v3)) ->
      let v1 = map_of_option map_qualifier v1
      and v2 = map_variable v2
      and v3 = map_paren_args (map_comma_list_arg map_argument) v3
      in FunCallVar ((v1, v2, v3))
  | StaticMethodCallSimple ((v1, v2, v3)) ->
      let v1 = map_qualifier v1
      and v2 = map_name v2
      and v3 = map_paren_args (map_comma_list_arg map_argument) v3
      in StaticMethodCallSimple ((v1, v2, v3))
  | MethodCallSimple ((v1, v2, v3, v4)) ->
      let v1 = map_variable v1
      and v2 = map_tok v2
      and v3 = map_name v3
      and v4 = map_paren_args (map_comma_list_arg map_argument) v4
      in MethodCallSimple ((v1, v2, v3, v4))
  | StaticMethodCallVar ((v1, v2, v3, v4)) ->
      let v1 = map_lvalue v1
      and v2 = map_tok v2
      and v3 = map_name v3
      and v4 = map_paren (map_comma_list map_argument) v4
      in StaticMethodCallVar ((v1, v2, v3, v4))
  | StaticObjCallVar ((v1, v2, v3, v4)) ->
      let v1 = map_lvalue v1
      and v2 = map_tok v2
      and v3 = map_lvalue v3
      and v4 = map_paren (map_comma_list map_argument) v4
      in StaticObjCallVar ((v1, v2, v3, v4))
  | ObjAccessSimple ((v1, v2, v3)) ->
      let v1 = map_variable v1
      and v2 = map_tok v2
      and v3 = map_name v3
      in ObjAccessSimple ((v1, v2, v3))
  | ObjAccess ((v1, v2)) ->
      let v1 = map_variable v1
      and v2 = map_obj_access v2
      in ObjAccess ((v1, v2))
and map_indirect =
  function | Dollar v1 -> let v1 = map_tok v1 in Dollar ((v1))
and map_argument =
  function
  | Arg v1 -> let v1 = map_expr v1 in Arg ((v1))
  | ArgRef ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_w_variable v2 in ArgRef ((v1, v2))
and map_obj_access (v1, v2, v3) =
  let v1 = map_tok v1
  and v2 = map_obj_property v2
  and v3 = map_of_option (map_paren_args (map_comma_list_arg map_argument)) v3
  in (v1, v2, v3)
and map_obj_property =
  function
  | ObjProp v1 -> let v1 = map_obj_dim v1 in ObjProp ((v1))
  | ObjPropVar v1 -> let v1 = map_variable v1 in ObjPropVar ((v1))
and map_obj_dim =
  function
  | OName v1 -> let v1 = map_name v1 in OName ((v1))
  | OBrace v1 -> let v1 = map_brace_expr map_expr v1 in OBrace ((v1))
  | OArrayAccess ((v1, v2)) ->
      let v1 = map_obj_dim v1
      and v2 = map_bracket (map_of_option map_expr) v2
      in OArrayAccess ((v1, v2))
  | OBraceAccess ((v1, v2)) ->
      let v1 = map_obj_dim v1
      and v2 = map_brace_expr map_expr v2
      in OBraceAccess ((v1, v2))
and map_rw_variable v = map_variable v
and map_r_variable v = map_variable v
and map_w_variable v = map_variable v
and map_stmt x =
  let rec k x = 
    match x with
  | ExprStmt ((v1, v2)) ->
      let v1 = map_expr v1 and v2 = map_tok v2 in ExprStmt ((v1, v2))
  | EmptyStmt v1 -> let v1 = map_tok v1 in EmptyStmt ((v1))
  | Block v1 ->
      let v1 = map_brace_body (map_of_list map_stmt_and_def) v1 in Block ((v1))
  | If ((v1, v2, v3, v4, v5)) ->
      let v1 = map_tok v1
      and v2 = map_paren_expr map_expr v2
      and v3 = map_stmt v3
      and v4 =
        map_of_list
          (fun (v1, v2, v3) ->
             let v1 = map_tok v1
             and v2 = map_paren_expr map_expr v2
             and v3 = map_stmt v3
             in (v1, v2, v3))
          v4
      and v5 =
        map_of_option
          (fun (v1, v2) ->
             let v1 = map_tok v1 and v2 = map_stmt v2 in (v1, v2))
          v5
      in If ((v1, v2, v3, v4, v5))
  | IfColon ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
      let v1 = map_tok v1
      and v2 = map_paren_expr map_expr v2
      and v3 = map_tok v3
      and v4 = map_of_list map_stmt_and_def v4
      and v5 = map_of_list map_new_elseif v5
      and v6 = map_of_option map_new_else v6
      and v7 = map_tok v7
      and v8 = map_tok v8
      in IfColon ((v1, v2, v3, v4, v5, v6, v7, v8))
  | While ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren_expr map_expr v2
      and v3 = map_colon_stmt v3
      in While ((v1, v2, v3))
  | Do ((v1, v2, v3, v4, v5)) ->
      let v1 = map_tok v1
      and v2 = map_stmt v2
      and v3 = map_tok v3
      and v4 = map_paren_expr map_expr v4
      and v5 = map_tok v5
      in Do ((v1, v2, v3, v4, v5))
  | For ((v1, v2, v3, v4, v5, v6, v7, v8, v9)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_for_expr v3
      and v4 = map_tok v4
      and v5 = map_for_expr v5
      and v6 = map_tok v6
      and v7 = map_for_expr v7
      and v8 = map_tok v8
      and v9 = map_colon_stmt v9
      in For ((v1, v2, v3, v4, v5, v6, v7, v8, v9))
  | Switch ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren_expr map_expr v2
      and v3 = map_switch_case_list v3
      in Switch ((v1, v2, v3))
  | Foreach ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_expr v3
      and v4 = map_tok v4
      and v5 = Ocaml.map_of_either map_foreach_variable map_variable v5
      and v6 = map_of_option map_foreach_arrow v6
      and v7 = map_tok v7
      and v8 = map_colon_stmt v8
      in Foreach ((v1, v2, v3, v4, v5, v6, v7, v8))
  | Break ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_expr v2
      and v3 = map_tok v3
      in Break ((v1, v2, v3))
  | Continue ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_expr v2
      and v3 = map_tok v3
      in Continue ((v1, v2, v3))
  | Return ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_expr v2
      and v3 = map_tok v3
      in Return ((v1, v2, v3))
  | Throw ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_expr v2
      and v3 = map_tok v3
      in Throw ((v1, v2, v3))
  | Try ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_brace_body (map_of_list map_stmt_and_def) v2
      and v3 = map_catch v3
      and v4 = map_of_list map_catch v4
      in Try ((v1, v2, v3, v4))
  | Echo ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list_expr map_expr v2
      and v3 = map_tok v3
      in Echo ((v1, v2, v3))
  | Globals ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list6 map_global_var v2
      and v3 = map_tok v3
      in Globals ((v1, v2, v3))
  | StaticVars ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list7 map_static_var v2
      and v3 = map_tok v3
      in StaticVars ((v1, v2, v3))
  | InlineHtml v1 -> let v1 = map_wrap_string map_of_string v1 in InlineHtml ((v1))
  | Use ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_use_filename v2
      and v3 = map_tok v3
      in Use ((v1, v2, v3))
  | Unset ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren8 (map_comma_list_var map_variable) v2
      and v3 = map_tok v3
      in Unset ((v1, v2, v3))
  | Declare ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren9 (map_comma_list8 map_declare) v2
      and v3 = map_colon_stmt v3
      in Declare ((v1, v2, v3))
  | TypedDeclaration ((v1, v2, v3, v4)) ->
      let v1 = map_hint_type v1
      and v2 = map_variable v2
      and v3 =
        map_of_option
          (fun (v1, v2) ->
             let v1 = map_tok v1 and v2 = map_expr v2 in (v1, v2))
          v3
      and v4 = map_tok v4
      in TypedDeclaration ((v1, v2, v3, v4))
  in
  vin.kstmt (k, all_functions) x

and map_switch_case_list =
  function
  | CaseList ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_tok v2
      and v3 = map_of_list map_case v3
      and v4 = map_tok v4
      in CaseList ((v1, v2, v3, v4))
  | CaseColonList ((v1, v2, v3, v4, v5)) ->
      let v1 = map_tok v1
      and v2 = map_of_option map_tok v2
      and v3 = map_of_list map_case v3
      and v4 = map_tok v4
      and v5 = map_tok v5
      in CaseColonList ((v1, v2, v3, v4, v5))
and map_case =
  function
  | Case ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_expr v2
      and v3 = map_tok v3
      and v4 = map_of_list map_stmt_and_def v4
      in Case ((v1, v2, v3, v4))
  | Default ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_tok v2
      and v3 = map_of_list map_stmt_and_def v3
      in Default ((v1, v2, v3))
and map_for_expr v = map_comma_list_expr map_expr v
and map_foreach_arrow (v1, v2) =
  let v1 = map_tok v1 and v2 = map_foreach_variable v2 in (v1, v2)
and map_foreach_variable (v1, v2) =
  let v1 = map_is_ref v1 and v2 = map_variable v2 in (v1, v2)
and map_catch (v1, v2, v3) =
  let v1 = map_tok v1
  and v2 =
    map_paren10
      (fun (v1, v2) ->
         let v1 = map_fully_qualified_class_name v1
         and v2 = map_dname v2
         in (v1, v2))
      v2
  and v3 = map_brace_body (map_of_list map_stmt_and_def) v3
  in (v1, v2, v3)
and map_use_filename =
  function
  | UseDirect v1 -> let v1 = map_wrap_string map_of_string v1 in UseDirect ((v1))
  | UseParen v1 ->
      let v1 = map_paren11 (map_wrap_string map_of_string) v1 in UseParen ((v1))
and map_declare (v1, v2) =
  let v1 = map_name v1 and v2 = map_static_scalar_affect v2 in (v1, v2)
and map_colon_stmt =
  function
  | SingleStmt v1 -> let v1 = map_stmt v1 in SingleStmt ((v1))
  | ColonStmt ((v1, v2, v3, v4)) ->
      let v1 = map_tok v1
      and v2 = map_of_list map_stmt_and_def v2
      and v3 = map_tok v3
      and v4 = map_tok v4
      in ColonStmt ((v1, v2, v3, v4))
and map_new_elseif (v1, v2, v3, v4) =
  let v1 = map_tok v1
  and v2 = map_paren_expr map_expr v2
  and v3 = map_tok v3
  and v4 = map_of_list map_stmt_and_def v4
  in (v1, v2, v3, v4)
and map_new_else (v1, v2, v3) =
  let v1 = map_tok v1
  and v2 = map_tok v2
  and v3 = map_of_list map_stmt_and_def v3
  in (v1, v2, v3)
and
  map_func_def {
                 f_tok = v_f_tok;
                 f_ref = v_f_ref;
                 f_name = v_f_name;
                 f_params = v_f_params;
                 f_body = v_f_body;
                 f_type = v_f_type;
                 f_return_type = v_f_return_type;
               } =
  let v_f_type = Type_php.map_phptype v_f_type in
  let v_f_body = map_brace_body (map_of_list map_stmt_and_def) v_f_body in
  let v_f_params = map_paren_params (map_comma_list_param map_parameter) v_f_params in
  let v_f_name = map_name v_f_name in
  let v_f_ref = map_is_ref v_f_ref in
  let v_f_tok = map_tok v_f_tok in
  let v_f_return_type = map_of_option map_hint_type v_f_return_type in
  {
    f_tok = v_f_tok;
    f_ref = v_f_ref;
    f_name = v_f_name;
    f_params = v_f_params;
    f_return_type = v_f_return_type;
    f_body = v_f_body;
    f_type = v_f_type;
  }
and
  map_parameter {
                  p_type = v_p_type;
                  p_ref = v_p_ref;
                  p_name = v_p_name;
                  p_default = v_p_default
                } =
  let v_p_default = map_of_option map_static_scalar_affect v_p_default in
  let v_p_name = map_dname v_p_name in
  let v_p_ref = map_is_ref v_p_ref in
  let v_p_type = map_of_option map_hint_type v_p_type in
  {
    p_type = v_p_type;
    p_ref = v_p_ref;
    p_name = v_p_name;
    p_default = v_p_default
  }

and map_hint_type =
  function
  | Hint v1 -> let v1 = map_fully_qualified_class_name v1 in Hint ((v1))
  | HintArray v1 -> let v1 = map_tok v1 in HintArray ((v1))
and map_is_ref v = map_of_option map_tok v
and
  map_lambda_def {
                   l_tok = v_l_tok;
                   l_ref = v_l_ref;
                   l_params = v_l_params;
                   l_use = v_l_use;
                   l_body = v_l_body
                 } =
  let v_l_body = map_brace_body (map_of_list map_stmt_and_def) v_l_body in
  let v_l_use = map_of_option map_lexical_vars v_l_use in
  let v_l_params = map_paren_params (map_comma_list_param map_parameter) v_l_params in
  let v_l_ref = map_is_ref v_l_ref in
  let v_l_tok = map_tok v_l_tok in 
  {
    l_tok = v_l_tok;
    l_ref = v_l_ref;
    l_params = v_l_params;
    l_use = v_l_use;
    l_body = v_l_body
  }

and map_lexical_vars (v1, v2) =
  let v1 = map_tok v1
  and v2 = map_paren_lexical_vars (map_comma_list10 map_lexical_var) v2
  in (v1, v2)
and map_lexical_var =
  function
  | LexicalVar ((v1, v2)) ->
      let v1 = map_is_ref v1 and v2 = map_dname v2 in LexicalVar ((v1, v2))
and
  map_class_def x =
  let rec k {
                  c_type = v_c_type;
                  c_name = v_c_name;
                  c_extends = v_c_extends;
                  c_implements = v_c_implements;
                  c_body = v_c_body
                } =
  let v_c_body = map_brace_class_body (map_of_list map_class_stmt) v_c_body in
  let v_c_implements = map_of_option map_interface v_c_implements in
  let v_c_extends = map_of_option map_extend v_c_extends in
  let v_c_name = map_name v_c_name in
  let v_c_type = map_class_type v_c_type in
  {
    c_type = v_c_type;
    c_name = v_c_name;
    c_extends = v_c_extends;
    c_implements = v_c_implements;
    c_body = v_c_body
  }
 in 
  vin.kclass_def (k, all_functions) x


and map_class_type =
  function
  | ClassRegular v1 -> let v1 = map_tok v1 in ClassRegular ((v1))
  | ClassFinal ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in ClassFinal ((v1, v2))
  | ClassAbstract ((v1, v2)) ->
      let v1 = map_tok v1 and v2 = map_tok v2 in ClassAbstract ((v1, v2))
and map_extend (v1, v2) =
  let v1 = map_tok v1 and v2 = map_fully_qualified_class_name v2 in (v1, v2)
and map_interface (v1, v2) =
  let v1 = map_tok v1
  and v2 = map_comma_list12 map_fully_qualified_class_name v2
  in (v1, v2)
and
  map_interface_def {
                      i_tok = v_i_tok;
                      i_name = v_i_name;
                      i_extends = v_i_extends;
                      i_body = v_i_body
                    } =
  let v_i_body = map_brace_class_body (map_of_list map_class_stmt) v_i_body in
  let v_i_extends = map_of_option map_interface v_i_extends in
  let v_i_name = map_name v_i_name in
  let v_i_tok = map_tok v_i_tok in
  {
    i_tok = v_i_tok;
    i_name = v_i_name;
    i_extends = v_i_extends;
    i_body = v_i_body
  }

and map_class_stmt =
  function
  | ClassConstants ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list13 map_class_constant v2
      and v3 = map_tok v3
      in ClassConstants ((v1, v2, v3))
  | ClassVariables ((v1, opt_ty, v2, v3)) ->
      let v1 = map_class_var_modifier v1
      and opt_ty = map_option map_hint_type opt_ty
      and v2 = map_comma_list14 map_class_variable v2
      and v3 = map_tok v3
      in ClassVariables ((v1, opt_ty, v2, v3))
  | Method v1 -> let v1 = map_method_def v1 in Method ((v1))
  | XhpDecl v1 -> let v1 = map_xhp_decl v1 in XhpDecl ((v1))


and map_xhp_decl =
  function
  | XhpAttributesDecl ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_xhp_attribute_decl v2
      and v3 = map_tok v3
      in XhpAttributesDecl ((v1, v2, v3))
  | XhpChildrenDecl ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_xhp_children_decl v2
      and v3 = map_tok v3
      in XhpChildrenDecl ((v1, v2, v3))
  | XhpCategoriesDecl ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_comma_list map_xhp_category_decl v2
      and v3 = map_tok v3
      in XhpCategoriesDecl ((v1, v2, v3))

and map_xhp_attribute_decl =
  function
  | XhpAttrInherit v1 ->
      let v1 = map_wrap_xhp_tag map_xhp_tag v1 in XhpAttrInherit ((v1))
  | XhpAttrDecl ((v1, v2, v3, v4)) ->
      let v1 = map_xhp_attribute_type v1
      and v2 = map_xhp_attr_name v2
      and v3 = map_of_option map_xhp_value_affect v3
      and v4 = map_of_option map_tok v4
      in XhpAttrDecl ((v1, v2, v3, v4))

and map_xhp_attribute_type =
  function
  | XhpAttrType v1 -> let v1 = map_name v1 in XhpAttrType ((v1))
  | XhpAttrEnum ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_brace_1 (map_comma_list map_constant) v2
      in XhpAttrEnum ((v1, v2))
and map_xhp_value_affect (v1, v2) =
  let v1 = map_tok v1 and v2 = map_constant v2 in (v1, v2)

and map_xhp_children_decl =
  function
  | XhpChild v1 -> let v1 = map_wrap_xhp_tag map_xhp_tag v1 in XhpChild ((v1))
  | XhpChildCategory v1 ->
      let v1 = map_wrap_xhp_tag map_xhp_tag v1 in XhpChildCategory ((v1))
  | XhpChildAny v1 -> let v1 = map_tok v1 in XhpChildAny ((v1))
  | XhpChildEmpty v1 -> let v1 = map_tok v1 in XhpChildEmpty ((v1))
  | XhpChildPcdata v1 -> let v1 = map_tok v1 in XhpChildPcdata ((v1))
  | XhpChildSequence ((v1, v2, v3)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      and v3 = map_xhp_children_decl v3
      in XhpChildSequence ((v1, v2, v3))
  | XhpChildAlternative ((v1, v2, v3)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      and v3 = map_xhp_children_decl v3
      in XhpChildAlternative ((v1, v2, v3))
  | XhpChildMul ((v1, v2)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      in XhpChildMul ((v1, v2))
  | XhpChildOption ((v1, v2)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      in XhpChildOption ((v1, v2))
  | XhpChildPlus ((v1, v2)) ->
      let v1 = map_xhp_children_decl v1
      and v2 = map_tok v2
      in XhpChildPlus ((v1, v2))
  | XhpChildParen v1 ->
      let v1 = map_paren map_xhp_children_decl v1 in XhpChildParen ((v1))
and map_xhp_category_decl v = map_wrap_xhp_tag map_xhp_tag v


and map_class_constant (v1, v2) =
  let v1 = map_name v1 and v2 = map_static_scalar_affect v2 in (v1, v2)
and map_class_variable (v1, v2) =
  let v1 = map_dname v1
  and v2 = map_of_option map_static_scalar_affect v2
  in (v1, v2)
and map_class_var_modifier =
  function
  | NoModifiers v1 -> let v1 = map_tok v1 in NoModifiers ((v1))
  | VModifiers v1 ->
      let v1 = map_of_list (map_wrap_modif map_modifier) v1 in VModifiers ((v1))
and
  map_method_def {
                   m_modifiers = v_m_modifiers;
                   m_tok = v_m_tok;
                   m_ref = v_m_ref;
                   m_name = v_m_name;
                   m_params = v_m_params;
                   m_return_type = v_m_return_type;
                   m_body = v_m_body
                 } =
  let v_m_body = map_method_body v_m_body in
  let v_m_params = map_paren_params (map_comma_list_param map_parameter) v_m_params in
  let v_m_name = map_name v_m_name in
  let v_m_ref = map_is_ref v_m_ref in
  let v_m_tok = map_tok v_m_tok in
  let v_m_modifiers = map_of_list (map_wrap_modif map_modifier) v_m_modifiers in
  let v_m_return_type = map_of_option map_hint_type v_m_return_type in

  {
    m_modifiers = v_m_modifiers;
    m_tok = v_m_tok;
    m_ref = v_m_ref;
    m_name = v_m_name;
    m_params = v_m_params;
    m_body = v_m_body;
    m_return_type = v_m_return_type;
  }

and map_modifier =
  function
  | Public -> Public
  | Private -> Private
  | Protected -> Protected
  | Static -> Static
  | Abstract -> Abstract
  | Final -> Final
and map_method_body =
  function
  | AbstractMethod v1 -> let v1 = map_tok v1 in AbstractMethod ((v1))
  | MethodBody v1 ->
      let v1 = map_brace_body (map_of_list map_stmt_and_def) v1
      in MethodBody ((v1))
and map_global_var =
  function
  | GlobalVar v1 -> let v1 = map_dname v1 in GlobalVar ((v1))
  | GlobalDollar ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_r_variable v2
      in GlobalDollar ((v1, v2))
  | GlobalDollarExpr ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_brace_expr map_expr v2
      in GlobalDollarExpr ((v1, v2))
and map_static_var (v1, v2) =
  let v1 = map_dname v1
  and v2 = map_of_option map_static_scalar_affect v2
  in (v1, v2)
and map_static_scalar =
  function
  | StaticConstant v1 -> let v1 = map_constant v1 in StaticConstant ((v1))
  | StaticClassConstant v1 ->
      let v1 =
        (match v1 with
         | (v1, v2) ->
             let v1 = map_qualifier v1 and v2 = map_name v2 in (v1, v2))
      in StaticClassConstant ((v1))
  | StaticPlus ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_static_scalar v2
      in StaticPlus ((v1, v2))
  | StaticMinus ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_static_scalar v2
      in StaticMinus ((v1, v2))
  | StaticArray ((v1, v2)) ->
      let v1 = map_tok v1
      and v2 = map_paren12 (map_comma_list15 map_static_array_pair) v2
      in StaticArray ((v1, v2))
  | XdebugStaticDots -> XdebugStaticDots
and map_static_scalar_affect (v1, v2) =
  let v1 = map_tok v1 and v2 = map_static_scalar v2 in (v1, v2)
and map_static_array_pair =
  function
  | StaticArraySingle v1 ->
      let v1 = map_static_scalar v1 in StaticArraySingle ((v1))
  | StaticArrayArrow ((v1, v2, v3)) ->
      let v1 = map_static_scalar v1
      and v2 = map_tok v2
      and v3 = map_static_scalar v3
      in StaticArrayArrow ((v1, v2, v3))
and map_stmt_and_def def =
  let rec k x = match x with
  | Stmt v1 -> let v1 = map_stmt v1 in Stmt ((v1))
  | FuncDefNested v1 -> let v1 = map_func_def v1 in FuncDefNested ((v1))
  | ClassDefNested v1 -> let v1 = map_class_def v1 in ClassDefNested ((v1))
  | InterfaceDefNested v1 ->
      let v1 = map_interface_def v1 in InterfaceDefNested ((v1))
  in
  vin.kstmt_and_def (k, all_functions) def
and map_toplevel =
  function
  | StmtList v1 -> let v1 = map_of_list map_stmt v1 in StmtList ((v1))
  | FuncDef v1 -> let v1 = map_func_def v1 in FuncDef ((v1))
  | ClassDef v1 -> let v1 = map_class_def v1 in ClassDef ((v1))
  | InterfaceDef v1 -> let v1 = map_interface_def v1 in InterfaceDef ((v1))
  | Halt ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_paren13 map_of_unit v2
      and v3 = map_tok v3
      in Halt ((v1, v2, v3))
  | NotParsedCorrectly v1 ->
      let v1 = map_of_list map_info v1 in NotParsedCorrectly ((v1))
  | FinalDef v1 -> let v1 = map_info v1 in FinalDef ((v1))
and map_program v = map_of_list map_toplevel v

and map_entity =
  function
  | FunctionE v1 -> let v1 = map_func_def v1 in FunctionE ((v1))
  | ClassE v1 -> let v1 = map_class_def v1 in ClassE ((v1))
  | InterfaceE v1 -> let v1 = map_interface_def v1 in InterfaceE ((v1))
  | StmtListE v1 -> let v1 = map_of_list map_stmt v1 in StmtListE ((v1))
  | MethodE v1 -> let v1 = map_method_def v1 in MethodE ((v1))
  | ClassConstantE v1 ->
      let v1 = map_class_constant v1 in ClassConstantE ((v1))
  | ClassVariableE ((v1, v2)) ->
      let v1 = map_class_variable v1
      and v2 = map_of_list map_modifier v2
      in ClassVariableE ((v1, v2))
  | XhpDeclE v1 -> let v1 = map_xhp_decl v1 in XhpDeclE ((v1))
  | MiscE v1 -> let v1 = map_of_list map_info v1 in MiscE ((v1))

and map_any =
  function
  | Lvalue v1 -> let v1 = map_variable v1 in Lvalue ((v1))
  | Expr v1 -> let v1 = map_expr v1 in Expr ((v1))
  | Stmt2 v1 -> let v1 = map_stmt v1 in Stmt2 ((v1))
  | StmtAndDef v1 -> let v1 = map_stmt_and_def v1 in StmtAndDef ((v1))
  | Toplevel v1 -> let v1 = map_toplevel v1 in Toplevel ((v1))
  | Program v1 -> let v1 = map_program v1 in Program ((v1))
  | Entity v1 -> let v1 = map_entity v1 in Entity ((v1))
  | Argument v1 -> let v1 = map_argument v1 in Argument ((v1))
  | Parameter v1 -> let v1 = map_parameter v1 in Parameter ((v1))
  | Parameters v1 ->
      let v1 = map_paren (map_comma_list map_parameter) v1
      in Parameters ((v1))
  | Body v1 ->
      let v1 = map_brace_body (map_of_list map_stmt_and_def) v1 in Body ((v1))
  | StmtAndDefs v1 ->
      let v1 = map_of_list map_stmt_and_def v1 in StmtAndDefs ((v1))
  | ClassStmt v1 -> let v1 = map_class_stmt v1 in ClassStmt ((v1))
  | ClassConstant2 v1 ->
      let v1 = map_class_constant v1 in ClassConstant2 ((v1))
  | ClassVariable v1 ->
      let v1 = map_class_variable v1 in ClassVariable ((v1))
  | ListAssign v1 -> let v1 = map_list_assign v1 in ListAssign ((v1))
  | ColonStmt2 v1 -> let v1 = map_colon_stmt v1 in ColonStmt2 ((v1))
  | XhpAttribute v1 -> let v1 = map_xhp_attribute v1 in XhpAttribute ((v1))
  | XhpAttrValue v1 -> let v1 = map_xhp_attr_value v1 in XhpAttrValue ((v1))
  | XhpHtml2 v1 -> let v1 = map_xhp_html v1 in XhpHtml2 ((v1))
  | Info v1 -> let v1 = map_info v1 in Info ((v1))
  | InfoList v1 -> let v1 = map_of_list map_info v1 in InfoList ((v1))
  | Case2 v1 -> let v1 = map_case v1 in Case2 ((v1))
  | StaticScalar v1 -> let v1 = map_static_scalar v1 in StaticScalar v1
  | Name2 v1 -> let v1 = map_name v1 in Name2 v1

 and all_functions =   
    {
      vtop = map_toplevel;
      vstmt_and_def = map_stmt_and_def;
      vprogram = map_program;
      vexpr = map_expr;
      vlvalue = map_variable;
      vxhpattrvalue = map_xhp_attr_value;
      vany = map_any;
    }
  in
  all_functions
