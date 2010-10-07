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

open Ast_ml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* hooks *)
type visitor_in = {
  kinfo: info vin;
  kexpr: expr vin;
}
and visitor_out = {
(*
  vexpr: expr  -> unit;
  vst: st -> unit;
  vtop: toplevel -> unit;
  vinfo: info -> unit;
  vprogram: program -> unit;
*)
  vtoplevel: toplevel vout;
  vprogram: program vout;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and 'a vout = 'a -> unit

let default_visitor = { 
  kinfo   = (fun (k,_) x -> k x);
  kexpr   = (fun (k,_) x -> k x);
}


let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

(* start of auto generation *)

let rec v_info x =
  let k x = match x with { Parse_info.
     token = v_pinfox; comments = v_comments; transfo = v_transfo 
    } ->
    let _arg = Parse_info.v_pinfo v_pinfox in
    let _arg = v_unit v_comments in 
    let _arg = Parse_info.v_transformation v_transfo in 
    ()
  in
  vin.kinfo (k, all_functions) x


and v_tok v = v_info v

and v_wrap _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap1 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()
and v_wrap2 _of_a (v1, v2) = let v1 = _of_a v1 and v2 = v_info v2 in ()

and v_paren _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren1 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_paren2 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()

and v_brace _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_brace1 _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()


and v_bracket _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in ()
and v_comma_list _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_comma_list1 _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_comma_list2 _of_a = v_list (Ocaml.v_either _of_a v_tok)

and v_and_list _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_and_list1 _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_and_list2 _of_a = v_list (Ocaml.v_either _of_a v_tok)

and v_pipe_list _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_pipe_list1 _of_a = v_list (Ocaml.v_either _of_a v_tok)

and v_semicolon_list _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_semicolon_list1 _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_semicolon_list2 _of_a = v_list (Ocaml.v_either _of_a v_tok)

and v_star_list _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_star_list1 _of_a = v_list (Ocaml.v_either _of_a v_tok)
and v_star_list2 _of_a = v_list (Ocaml.v_either _of_a v_tok)
  
and v_name = function | Name v1 -> let v1 = v_wrap1 v_string v1 in ()
and v_lname v = v_name v
and v_uname v = v_name v
  
and v_long_name (v1, v2) =
  let v1 = v_qualifier v1 and v2 = v_name v2 in ()
and v_qualifier v =
  v_list (fun (v1, v2) -> let v1 = v_name v1 and v2 = v_tok v2 in ()) v
  
and v_ty =
  function
  | TyName v1 -> let v1 = v_long_name v1 in ()
  | TyVar ((v1, v2)) -> let v1 = v_tok v1 and v2 = v_name v2 in ()
  | TyTuple v1 -> let v1 = v_star_list2 v_ty v1 in ()
  | TyFunction ((v1, v2, v3)) ->
      let v1 = v_ty v1 and v2 = v_tok v2 and v3 = v_ty v3 in ()
  | TyApp ((v1, v2)) -> let v1 = v_ty_args v1 and v2 = v_long_name v2 in ()
  | TyTodo -> ()

and v_type_declaration =
  function
  | TyAbstract ((v1, v2)) -> let v1 = v_ty_params v1 and v2 = v_name v2 in ()
  | TyDef ((v1, v2, v3, v4)) ->
      let v1 = v_ty_params v1
      and v2 = v_name v2
      and v3 = v_tok v3
      and v4 = v_type_def_kind v4
      in ()
and v_type_def_kind =
  function
  | TyCore v1 -> let v1 = v_ty v1 in ()
  | TyAlgebric v1 -> let v1 = v_pipe_list1 v_constructor_declaration v1 in ()
  | TyRecord v1 ->
      let v1 = v_brace1 (v_semicolon_list2 v_label_declaration) v1 in ()
and v_constructor_declaration (v1, v2) =
  let v1 = v_name v1 and v2 = v_constructor_arguments v2 in ()
and v_constructor_arguments =
  function
  | NoConstrArg -> ()
  | Of ((v1, v2)) -> let v1 = v_tok v1 and v2 = v_star_list1 v_ty v2 in ()
and
  v_label_declaration {
                        fld_mutable = v_fld_mutable;
                        fld_name = v_fld_name;
                        fld_tok = v_fld_tok;
                        fld_type = v_fld_type
                      } =
  let arg = v_option v_tok v_fld_mutable in
  let arg = v_name v_fld_name in
  let arg = v_tok v_fld_tok in let arg = v_ty v_fld_type in ()
and v_ty_args =
  function
  | TyArg1 v1 -> let v1 = v_ty v1 in ()
  | TyArgMulti v1 -> let v1 = v_paren2 (v_comma_list2 v_ty) v1 in ()
and v_ty_params =
  function
  | TyNoParam -> ()
  | TyParam1 v1 -> let v1 = v_ty_parameter v1 in ()
  | TyParamMulti v1 ->
      let v1 = v_paren1 (v_comma_list1 v_ty_parameter) v1 in ()
and v_ty_parameter (v1, v2) = let v1 = v_tok v1 and v2 = v_name v2 in ()
and v_expr v = v_unit v
and v_seq_expr v = v_semicolon_list1 v_expr v
and v_pattern v = v_unit v
and v_simple_pattern v = v_unit v
and v_labeled_simple_pattern v = v_unit v
and v_let_binding =
  function
  | LetClassic v1 -> let v1 = v_let_def v1 in ()
  | LetPattern ((v1, v2, v3)) ->
      let v1 = v_pattern v1 and v2 = v_tok v2 and v3 = v_seq_expr v3 in ()
and
  v_let_def {
              l_name = v_l_name;
              l_args = v_l_args;
              l_tok = v_l_tok;
              l_body = v_l_body
            } =
  let arg = v_name v_l_name in
  let arg = v_list v_labeled_simple_pattern v_l_args in
  let arg = v_tok v_l_tok in let arg = v_seq_expr v_l_body in ()
and v_function_def v = v_unit v
and v_module_type v = v_unit v
and v_module_expr v = v_unit v
and v_item =
  function
  | Type ((v1, v2)) ->
      let v1 = v_tok v1 and v2 = v_and_list2 v_type_declaration v2 in ()
  | Exception ((v1, v2, v3)) ->
      let v1 = v_tok v1
      and v2 = v_name v2
      and v3 = v_constructor_arguments v3
      in ()
  | External ((v1, v2, v3, v4, v5, v6)) ->
      let v1 = v_tok v1
      and v2 = v_name v2
      and v3 = v_tok v3
      and v4 = v_ty v4
      and v5 = v_tok v5
      and v6 = v_list (v_wrap2 v_string) v6
      in ()
  | Open ((v1, v2)) -> let v1 = v_tok v1 and v2 = v_long_name v2 in ()
  | Val ((v1, v2, v3, v4)) ->
      let v1 = v_tok v1
      and v2 = v_name v2
      and v3 = v_tok v3
      and v4 = v_ty v4
      in ()
  | Let ((v1, v2, v3)) ->
      let v1 = v_tok v1
      and v2 = v_rec_opt v2
      and v3 = v_and_list1 v_let_binding v3
      in ()
  | ItemTodo -> ()
and v_sig_item v = v_item v
and v_struct_item v = v_item v
and v_rec_opt v = v_option v_tok v


and v_toplevel =
  function
  | Item v1 -> let v1 = v_item v1 in ()
  | ScSc v1 -> let v1 = v_info v1 in ()
  | TopSeqExpr v1 -> let v1 = v_seq_expr v1 in ()
  | NotParsedCorrectly v1 -> let v1 = v_list v_info v1 in ()
  | FinalDef v1 -> let v1 = v_info v1 in ()
and v_program v = v_list v_toplevel v
  
 and all_functions =   
    {
      vprogram = v_program;
      vtoplevel = v_toplevel;
    }
  in
  all_functions
