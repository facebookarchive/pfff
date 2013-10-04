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
open Ast_js

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

module CommonX = struct
    let v_parse_info x = ()
    let v_either of_a of_b x = 
      match x with
      | Common.Left a -> of_a a
      | Common.Right b -> of_b b
end 

(* hooks *)
type visitor_in = {

  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (st  -> unit) * visitor_out -> st  -> unit;
  kfield: (field -> unit) * visitor_out -> field -> unit;
  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
and visitor_out = any -> unit

let default_visitor = 
  { kexpr   = (fun (k,_) x -> k x);
    kstmt   = (fun (k,_) x -> k x);
    kinfo   = (fun (k,_) x -> k x);

    kfield = (fun (k,_) x -> k x);
  }

let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

(* start of auto generation *)


let rec v_info x =
  let k x = match x with { Parse_info.
     token = v_pinfox; transfo = v_transfo 
    } ->
(*
    let arg = Parse_info.v_pinfo v_pinfox in
    let arg = v_unit v_comments in 
    let arg = Parse_info.v_transformation v_transfo in 
*)
    ()
  in
  vin.kinfo (k, all_functions) x

and v_tok v = v_info v

and v_wrap _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap2 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap3 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap4 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap5 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap6 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap7 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap8 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap9 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap10 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap11 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()


and v_paren _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren2 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren3 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren4 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren5 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren6 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren7 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren8 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren9 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren10 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()


and v_brace: 'a. ('a -> unit) -> 'a brace -> unit = fun _of_a (v1, v2, v3) ->
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()

and v_brace2 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_brace3 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_brace4 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_brace5 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_brace6 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()

and v_bracket _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_bracket2 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()


and v_comma x = v_tok x

and v_comma_list _of_a = v_list (CommonX.v_either _of_a v_tok)
and v_comma_list2 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list3 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list4 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list5 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list6 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list7 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list8 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list9 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)

and v_comma_list10 _of_a xs = 
  xs +> List.iter (function | Left x -> _of_a x | Right info -> v_comma info)


and v_sc v = v_option v_tok v

and v_name v = v_wrap v_string v
  
and v_expr (x: expr) = 
  (* tweak *)
  let k x =  match x with
  | L v1 -> let v1 = v_litteral v1 in ()
  | V v1 -> let v1 = v_name v1 in ()
  | This v1 -> let v1 = v_tok v1 in ()
  | U ((v1, v2)) -> let v1 = v_wrap2 v_unop v1 and v2 = v_expr v2 in ()
  | B ((v1, v2, v3)) ->
      let v1 = v_expr v1 and v2 = v_wrap3 v_binop v2 and v3 = v_expr v3 in ()
  | Bracket ((v1, v2)) ->
      let v1 = v_expr v1 and v2 = v_bracket v_expr v2 in ()
  | Period ((v1, v2, v3)) ->
      let v1 = v_expr v1 and v2 = v_tok v2 and v3 = v_name v3 in ()
  | Object v1 ->
      let v1 =
        v_brace
          (v_comma_list v_field

             )
          v1
      in ()
  | Array v1 -> let v1 = v_bracket2 (v_comma_list2 v_expr) v1 in ()
  | Apply ((v1, v2)) ->
      let v1 = v_expr v1 and v2 = v_paren (v_comma_list2 v_expr) v2 in ()
  | Conditional ((v1, v2, v3, v4, v5)) ->
      let v1 = v_expr v1
      and v2 = v_tok v2
      and v3 = v_expr v3
      and v4 = v_tok v4
      and v5 = v_expr v5
      in ()
  | Assign ((v1, v2, v3)) ->
      let v1 = v_expr v1
      and v2 = v_wrap4 v_assignment_operator v2
      and v3 = v_expr v3
      in ()
  | Seq ((v1, v2, v3)) ->
      let v1 = v_expr v1 and v2 = v_tok v2 and v3 = v_expr v3 in ()
  | Function v1 -> let v1 = v_func_decl v1 in ()
  | Extra v1 -> let v1 = v_extra v1 in ()
  | Paren v1 -> let v1 = v_paren2 v_expr v1 in ()
  in
  vin.kexpr (k, all_functions) x 

and v_field x = 
  let rec k x = 
    let (v1, v2, v3) = x in

    let v1 = v_property_name v1
    and v2 = v_tok v2
    and v3 = v_expr v3
    in ()
  in
  vin.kfield (k, all_functions) x

and v_extra = function | DanglingComma -> ()
and v_litteral =
  function
  | Bool v1 -> let v1 = v_wrap5 v_bool v1 in ()
  | Num v1 -> let v1 = v_wrap6 v_string v1 in ()
  | String v1 -> let v1 = v_wrap6 v_string v1 in ()
  | Regexp v1 -> let v1 = v_wrap6 v_string v1 in ()
  | Null v1 -> let v1 = v_tok v1 in ()
  | Undefined -> ()
and v_unop =
  function
  | U_new -> ()
  | U_delete -> ()
  | U_void -> ()
  | U_typeof -> ()
  | U_bitnot -> ()
  | U_pre_increment -> ()
  | U_pre_decrement -> ()
  | U_post_increment -> ()
  | U_post_decrement -> ()
  | U_plus -> ()
  | U_minus -> ()
  | U_not -> ()
and v_binop =
  function
  | B_instanceof -> ()
  | B_in -> ()
  | B_mul -> ()
  | B_div -> ()
  | B_mod -> ()
  | B_add -> ()
  | B_sub -> ()
  | B_le -> ()
  | B_ge -> ()
  | B_lt -> ()
  | B_gt -> ()
  | B_lsr -> ()
  | B_asr -> ()
  | B_lsl -> ()
  | B_equal -> ()
  | B_notequal -> ()
  | B_physequal -> ()
  | B_physnotequal -> ()
  | B_bitand -> ()
  | B_bitor -> ()
  | B_bitxor -> ()
  | B_and -> ()
  | B_or -> ()
and v_property_name =
  function
  | PN_String v1 -> let v1 = v_name v1 in ()
  | PN_Num v1 -> let v1 = v_wrap v_string v1 in ()
  | PN_Empty -> ()
and v_assignment_operator =
  function
  | A_eq -> ()
  | A_mul -> ()
  | A_div -> ()
  | A_mod -> ()
  | A_add -> ()
  | A_sub -> ()
  | A_lsl -> ()
  | A_lsr -> ()
  | A_asr -> ()
  | A_and -> ()
  | A_xor -> ()
  | A_or -> ()
and v_st x =
  let rec k x = match x with
  | Variable ((v1, v2, v3)) ->
      let v1 = v_tok v1
      and v2 = v_comma_list3 v_variable_declaration v2
      and v3 = v_sc v3
      in ()
  | Const ((v1, v2, v3)) ->
      let v1 = v_tok v1
      and v2 = v_comma_list3 v_variable_declaration v2
      and v3 = v_sc v3
      in ()
  | Block v1 -> let v1 = v_brace2 (v_list v_toplevel) v1 in ()
  | Nop v1 -> let v1 = v_sc v1 in ()
  | ExprStmt ((v1, v2)) -> let v1 = v_expr v1 and v2 = v_sc v2 in ()
  | If ((v1, v2, v3, v4)) ->
      let v1 = v_tok v1
      and v2 = v_paren2 v_expr v2
      and v3 = v_st v3
      and v4 =
        v_option (fun (v1, v2) -> let v1 = v_tok v1 and v2 = v_st v2 in ())
          v4
      in ()
  | Do ((v1, v2, v3, v4, v5)) ->
      let v1 = v_tok v1
      and v2 = v_st v2
      and v3 = v_tok v3
      and v4 = v_paren2 v_expr v4
      and v5 = v_sc v5
      in ()
  | While ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_paren2 v_expr v2 and v3 = v_st v3 in ()
  | For ((v1, v2, v3, v4, v5, v6, v7, v8, v9)) ->
      let v1 = v_tok v1
      and v2 = v_tok v2
      and v3 = v_option v_lhs_or_var v3
      and v4 = v_tok v4
      and v5 = v_option v_expr v5
      and v6 = v_tok v6
      and v7 = v_option v_expr v7
      and v8 = v_tok v8
      and v9 = v_st v9
      in ()
  | ForIn ((v1, v2, v3, v4, v5, v6, v7)) ->
      let v1 = v_tok v1
      and v2 = v_tok v2
      and v3 = v_lhs_or_var v3
      and v4 = v_tok v4
      and v5 = v_expr v5
      and v6 = v_tok v6
      and v7 = v_st v7
      in ()
  | Switch ((v1, v2, v3)) ->
      let v1 = v_tok v1
      and v2 = v_paren2 v_expr v2
      and v3 = v_brace3 (v_list v_case_clause) v3
      in ()
  | Continue ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_option v_label v2 and v3 = v_sc v3 in ()
  | Break ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_option v_label v2 and v3 = v_sc v3 in ()
  | Return ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_option v_expr v2 and v3 = v_sc v3 in ()
  | With ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_paren2 v_expr v2 and v3 = v_st v3 in ()
  | Labeled ((v1, v2, v3)) ->
      let v1 = v_label v1 and v2 = v_tok v2 and v3 = v_st v3 in ()
  | Throw ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_expr v2 and v3 = v_sc v3 in ()
  | Try ((v1, v2, v3, v4)) ->
      let v1 = v_tok v1
      and v2 = v_st v2
      and v3 =
        v_option
          (fun (v1, v2, v3) ->
             let v1 = v_tok v1
             and v2 = v_paren3 v_arg v2
             and v3 = v_st v3
             in ())
          v3
      and v4 =
        v_option (fun (v1, v2) -> let v1 = v_tok v1 and v2 = v_st v2 in ())
          v4
      in ()
  in
  vin.kstmt (k, all_functions) x

and v_label v = v_wrap v_string v
and v_lhs_or_var =
  function
  | LHS v1 -> let v1 = v_expr v1 in ()
  | Vars ((v1, v2)) ->
      let v1 = v_tok v1 and v2 = v_comma_list3 v_variable_declaration v2 in ()
and v_case_clause =
  function
  | Default ((v1, v2, v3)) ->
      let v1 = v_tok v1 and v2 = v_tok v2 and v3 = v_list v_toplevel v3 in ()
  | Case ((v1, v2, v3, v4)) ->
      let v1 = v_tok v1
      and v2 = v_expr v2
      and v3 = v_tok v3
      and v4 = v_list v_toplevel v4
      in ()
and v_arg v = v_wrap v_string v
and v_func_decl (v1, v2, v3, v4) =
  let v1 = v_option v_tok v1
  and v2 = v_option v_name v2
  and v3 = v_paren4 (v_comma_list4 v_name) v3
  and v4 = v_brace4 (v_list v_toplevel) v4
  in ()
and v_variable_declaration (v1, v2) =
  let v1 = v_name v1
  and v2 =
    v_option (fun (v1, v2) -> let v1 = v_tok v1 and v2 = v_expr v2 in ()) v2
  in ()
and
  v_class_decl {
                 c_tok = v_c_tok;
                 c_name = v_c_name;
                 c_extends = v_c_extends;
                 c_body = v_c_body
               } =
  let arg = v_tok v_c_tok in
  let arg = v_name v_c_name in
  let arg =
    v_option
      (fun (v1, v2) -> let v1 = v_tok v1 and v2 = v_inherit_expr v2 in ())
      v_c_extends in
  let arg = v_brace (v_list v_class_stmt) v_c_body in 
  ()
and v_class_stmt =
  function
  | Method ((v1, v2)) ->
      let v1 = v_option v_tok v1 and v2 = v_func_decl v2 in ()
  | ClassExtraSemiColon v1 -> let v1 = v_sc v1 in ()
and v_inherit_expr v = v_expr v

and v_toplevel =
  function
  | St v1 -> let v1 = v_st v1 in ()
  | FunDecl v1 -> let v1 = v_func_decl v1 in ()
  | ClassDecl v1 -> let v1 = v_class_decl v1 in ()
  | NotParsedCorrectly v1 -> let v1 = v_list v_info v1 in ()
  | FinalDef v1 -> let v1 = v_info v1 in ()
and v_program v = v_list v_toplevel v

and v_any =  function
  | Expr v1 -> let v1 = v_expr v1 in ()
  | Stmt v1 -> let v1 = v_st v1 in ()
  | Func v1 -> let v1 = v_func_decl v1 in ()
  | Toplevel v1 -> let v1 = v_toplevel v1 in ()
  | Program v1 -> let v1 = v_program v1 in ()

and all_functions x = v_any x
in
all_functions

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let do_visit_with_ref mk_hooks = fun any ->
  let res = ref [] in
  let hooks = mk_hooks res in
  let vout = mk_visitor hooks in
  vout any;
  List.rev !res
