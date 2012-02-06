(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module TV = Token_views_opa
module TH = Token_helpers_opa
module T = Parser_opa

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Parsing OPA turns out to be difficult. The grammar is not yacc-able.
 * There is a degenerated AST in token_views_opa.ml, essentially a
 * tree of (){}[]<tag chunks. This tree helps, using heuristics, to
 * identify function names, types, etc. This module brings a little
 * bit more organization over this degenerated AST.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a wrap = 'a Ast_opa.wrap

type name = 
  Name of string wrap
 (* with tarzan *)

type long_name = qualifier * name
 and qualifier = name list
 (* with tarzan *)

type tree =
  | Function of func_def

  (* Database of type_ option * path * value_ option *)
  (* TypeDef of name * type_ option *)
  (* Package of ... *)
  (* Module of name * tree list *)
  (* VarDef of type_option * name * value_ *)

  | TreeTodo
  (* a copy of Token_views_opa.tree *)
  | T of Parser_opa.token
  | Paren of tree list list
  | Brace of tree list list
  | Bracket of tree list list
  | Xml of tree list * tree list

 and func_def = {
   (* f_name = None when have lambda *)
   f_name: name option;
   f_ret_type: type_ option;
   f_params: parameter list;
   f_body: body;
 }
   and parameter =
     | Param of type_ option * name
     | ParamOther of tree list

  and body = tree list

  and type_ =
    | TyName of long_name
    | TyVar of (* ' *) name
    | TyApp of long_name * type_ list
    | TyOther of tree list
 (* with tarzan *)

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

(*****************************************************************************)
(* Builder *)
(*****************************************************************************)

let toks_for_ast_fuzzy toks = toks +> Common.exclude (function
  | x when TH.is_comment x -> true
    (* todo? could try to relocate the following token to column 0? *)
    | T.Tclient _ | T.Tserver _ -> true
    | T.Tpublic _ | T.Tprivate _ -> true
    | T.Tprotected _ | T.Texposed _ -> true
    | _ -> false
  )


let (mk_tree: TV.tree list -> tree list) = fun xs ->
  
  let top_ctx = () in

  (* poor's man parser ... *)
  let rec tree ctx = function

  | TV.T tok -> T tok
  | TV.Paren xxs ->
      let xxs = xxs +> List.map (tree_list ctx) in
      Paren xxs
  | TV.Brace xxs ->
      let xxs = xxs +> List.map (tree_list ctx) in
      Brace xxs
  | TV.Bracket xxs ->
      let xxs = xxs +> List.map (tree_list ctx) in
      Bracket xxs
  | TV.Xml ((v1, v2)) ->
      raise Todo

  and tree_list ctx xs = 

    match xs with
    | [] -> []
    (*-------------------------------------------------------------------*)
    (* functions *)
    (*-------------------------------------------------------------------*)

    (* function x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)::(TV.Brace bdy)
      ::xs ->
        Function ({
          f_ret_type = None;
          f_name = Some (Name (s1, ii1));
          f_params = List.map (parameter ctx) params;
          f_body = body ctx bdy;
        })::tree_list ctx xs

    (* function yy x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s0, ii0))
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)::(TV.Brace bdy)
      ::xs ->
        Function ({
          f_ret_type = Some (type_ ctx [(TV.T (T.TIdent (s0, ii0)))]);
          f_name = Some (Name (s1, ii1));
          f_params = List.map (parameter ctx) params;
          f_body = body ctx bdy;
        })::tree_list ctx xs

    (* function yy(zz) x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s0, ii0))
      ::(TV.Paren paramstype)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)::(TV.Brace bdy)
      ::xs ->
        Function ({
          f_ret_type = Some 
            (type_ ctx [(TV.T (T.TIdent (s0, ii0)));(TV.Paren paramstype)]);
          f_name = Some (Name (s1, ii1));
          f_params = List.map (parameter ctx) params;
          f_body = body ctx bdy;
        })::tree_list ctx xs


    (*-------------------------------------------------------------------*)
    (* Database *)
    (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* Types *)
    (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* Modules/packages *)
    (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* Record *)
    (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* String interpolation *)
    (*-------------------------------------------------------------------*)

(*
    (* function (...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.Paren params)
      ::(TV.Brace body)
      ::xs ->
        List.iter (aux_tree InParameter) params;
        aux_tree InFunction [(TV.Brace body)];
        aux_tree ctx xs

    (* function (...) (...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.Paren paramstype)
      ::(TV.Paren params)
      ::(TV.Brace body)
      ::xs ->
        aux_tree InType [(TV.Paren paramstype)];
        List.iter (aux_tree InParameter) params;
        aux_tree InFunction [(TV.Brace body)];
        aux_tree ctx xs


    (* database yy /x *)
    |   (TV.T T.Tdatabase _)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.T T.TDiv _)
      ::(TV.T T.TIdent (s2, ii2))
      ::xs ->
        aux_tree InType [(TV.T (T.TIdent (s1, ii1)))];
        tag ii2 (Global (Def2 fake_no_def2));
        aux_tree ctx xs


    (* database yy(zz) /x *)
    |   (TV.T T.Tdatabase _)
      ::(TV.T T.TIdent (s0, ii0))
      ::(TV.Paren paramstype)
      ::(TV.T T.TDiv _)
      ::(TV.T T.TIdent (s2, ii2))
      ::xs ->
        aux_tree InType [(TV.T (T.TIdent (s0, ii0)));(TV.Paren paramstype)];
        tag ii2 (Global (Def2 fake_no_def2));
        aux_tree ctx xs

    (* database /xxx *)
    |   (TV.T T.Tdatabase _)
      ::(TV.T T.TDiv _)
      ::(TV.T T.TIdent (s, ii1))
      ::xs ->
        tag ii1 (Global (Def2 fake_no_def2));
        aux_tree ctx xs

    (* type x = { ... } *)
    |   (TV.T T.Ttype _)
      ::(TV.T (T.TIdent (s, ii1)))
      ::(TV.T (T.TEq ii2))
      ::TV.Brace bodytype
      ::xs ->
        tag ii1 (TypeDef Def);
        List.iter (aux_tree InTypedef) bodytype;
        aux_tree ctx xs

    (* todo: type x(yy) = *)

    (* todo? package ... *)
    (* todo? module x = {...} *)

    (* todo? x = ... at toplevel *)


    (* INSIDE Typedef *)
    
    (* yy x *)
    |  (TV.T T.TIdent (s1, ii1))
     ::(TV.T T.TIdent (s2, ii2))
     ::xs when ctx = InTypedef ->
       aux_tree InType [(TV.T (T.TIdent (s1, ii1)))];
       tag ii2 (Field (Def2 fake_no_def2));
       aux_tree ctx xs

    (* yy(zz) x *)
    |  (TV.T T.TIdent (s1, ii1))
     ::(TV.Paren paramstype)
     ::(TV.T T.TIdent (s2, ii2))
     ::xs when ctx = InTypedef ->
        aux_tree InType [(TV.T (T.TIdent (s1, ii1)));(TV.Paren paramstype)];
        tag ii2 (Field (Def2 fake_no_def2));
        aux_tree ctx xs

    (* INSIDE Function *)
    |  (TV.T (T.TIdent (s1, ii1)))
     ::(TV.T (T.TEq _))
     ::xs when ctx = InFunction ->
       tag ii1 (Local Def);
       aux_tree ctx xs

    (* INSIDE Top *)

    |  (TV.T (T.TIdent (s1, ii1)))
     ::(TV.T (T.TEq _))
     ::(TV.T (T.TExternalIdent (s2, ii2)))
     ::xs when ctx = InTop ->
       tag ii1 (Function (Def2 fake_no_def2));
       tag ii2 CppOther;
       aux_tree ctx xs

    |  (TV.T (T.TIdent (s1, ii1)))
     ::(TV.T (T.TEq _))
     ::xs when ctx = InTop ->
       tag ii1 (Global (Def2 fake_no_def2));
       aux_tree ctx xs

       *)
    | x::xs ->
        tree ctx x::tree_list ctx xs

  and type_ ctx xs =
    match xs with
    | [(TV.T T.TIdent (s1, ii1))] -> 
        TyName ([], Name (s1, ii1))
    | [(TV.T T.TIdent (s1, ii1));TV.Paren paramstype] -> 
        TyApp (([], Name (s1, ii1)), 
               List.map (type_ ctx) paramstype)
    | xs -> 
        TyOther (tree_list ctx xs)

  and parameter ctx param = 
    match param with
    (* x *)
    |  [(TV.T T.TIdent (s1, ii1))] ->
         Param (None, Name (s1, ii1))

    (* yy x *)
    |  [(TV.T T.TIdent (s1, ii1));(TV.T T.TIdent (s2, ii2))] ->
         Param (Some (type_ ctx [(TV.T (T.TIdent (s1, ii1)))]), 
               Name (s2, ii2))

    (* yy(zz) x *)
    |  [(TV.T T.TIdent (s1, ii1));(TV.Paren paramstype)
       ;(TV.T T.TIdent (s2, ii2))] ->
         Param (Some (type_ ctx 
                         [(TV.T (T.TIdent (s1, ii1)));(TV.Paren paramstype)]),
               Name (s2, ii2))

    | xs -> ParamOther (tree_list ctx xs)

  and body ctx body =
    match body with
    | [] -> []
    | [xs] -> tree_list ctx xs
    | x::y::xs -> failwith "the body should have no comma"
  in


  tree_list top_ctx xs
