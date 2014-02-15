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

module T = Parser_opa
module TH = Token_helpers_opa
module TV = Token_views_opa

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Parsing OPA turns out to be difficult. The grammar is not yacc compliant.
 * There is a tree of tokens in token_views_opa.ml, essentially a
 * tree of (){}[]<tag chunks. This tree helps, using heuristics encoded
 * in this file, to identify function names, types, etc. This module
 * thus brings a little bit more organization over this tree by having 
 * a kind of fuzzy AST.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a wrap = 'a Ast_opa.wrap

(* ------------------------------------------------------------------------- *)
(* Names *)
(* ------------------------------------------------------------------------- *)

type name = 
  Name of string wrap
 (* with tarzan *)

type long_name = qualifier * name
 and qualifier = name list
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Top *)
(* ------------------------------------------------------------------------- *)

(* 
 * Many symbols in OPA are "overloaded" which requires to use some context
 * and heuristics to identify AST elements:
 *
 * - '{}' are used for: funcdef, module def, TODO compound,
 *   record def, TODO algebraic def, TODO interpolation, TODO records,
 *   TODO record patterns
 *
 * - '()' are used for: TODO funcall, TODO tuples,  type application,
 *    TODO polymorphic types
 * 
 * - '[]' are used for: TODO list, TODO parser character classes
 *
 * - '.' used for: module access, TODO field access
 *
 * - '=' is used for: typedef, variable def (and for function def
 *    in classic syntax)
 *)
type tree =
  | Function of func_def
  | TypeDef of name * type_def
  (* todo: Database of type_ option * path * value_ option *)

  | VarDef of type_ option * name (* todo: value_ *)

  (* todo: Package of ... *)
  | Module of name * tree list

  | TreeTodo

  (* a copy of Token_views_opa.tree *)
  | T of Parser_opa.token
  | Paren of tree list list
  | Brace of tree list list
  | Bracket of tree list list
  (* todo *)
  | Xml of tree list * tree list
  (* todo: String of encaps list *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

 and type_ =
   | TyName of long_name
   | TyVar of (* ' *) name
   | TyApp of long_name * type_ list

   | TyOther of tree list

 and type_def =
   | TyRecord of field_decl list
   (* TyAlgebric of ctor_decl list *)

   | TypeDefOther of tree list

 and field_decl =
   | Field of type_ option * name

   | FieldOther of tree list

(* ------------------------------------------------------------------------- *)
(* Function def *)
(* ------------------------------------------------------------------------- *)

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

 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let info_of_name (Name (_s, ii)) = ii
let str_of_name (Name (s, _ii))  = s

let is_module_name s =
  s =~ "[A-Z].*"

(* skipping comments, qualifiers, annotations *)
let toks_for_ast_fuzzy toks =
  let toks = toks +> Common.exclude TH.is_comment in
  let rec aux toks =
    match toks with
    | [] -> []
    (* todo? could try to relocate the following token to column 0? *)
    | (
        T.Tclient _ | T.Tserver _
      | T.Tpublic _ | T.Tprivate _ 
      | T.Tprotected _ | T.Texposed _
      )::xs -> aux xs
    (* similar to what I do for c++, skipping templates and qualifiers *)
    | T.TIdent (s, _i)::T.TDot _::xs when is_module_name s ->
        aux xs
    | T.TAt _::T.TIdent _::xs ->
        aux xs

    | x::xs -> x::aux xs
  in
  aux toks

(*****************************************************************************)
(* Builder *)
(*****************************************************************************)

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
  | TV.Xml ((_, _)) ->
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

    (* function (...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.Paren params)::(TV.Brace bdy)::xs ->
        Function ({
          f_ret_type = None;
          f_name = None;
          f_params = List.map (parameter ctx) params;
          f_body = body ctx bdy;
        })::tree_list ctx xs

    (* function (...) (...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.Paren [typ])
      ::(TV.Paren params)::(TV.Brace bdy)
      ::xs ->
        Function ({
          f_ret_type = Some (type_ ctx typ);
          f_name = None;
          f_params = List.map (parameter ctx) params;
          f_body = body ctx bdy;
        })::tree_list ctx xs

    (*-------------------------------------------------------------------*)
    (* Type defs *)
    (*-------------------------------------------------------------------*)
    (* todo: type x = { ... } or ... *)

    (* type x = { ... } *)
    |   (TV.T T.Ttype _)
      ::(TV.T (T.TIdent (s, ii1)))
      ::(TV.T (T.TEq _))
      ::TV.Brace bodytype
      ::xs ->
        (TypeDef (Name (s, ii1),
                TyRecord (List.map (field_decl ctx) bodytype)))
          ::tree_list ctx xs

    (* todo: type x(yy) = *)

    (*-------------------------------------------------------------------*)
    (* Modules/packages *)
    (*-------------------------------------------------------------------*)
    (* todo? module x = {...} *)
    |   (TV.T T.Tmodule _)
      ::(TV.T (T.TIdent (s, ii1)))
      ::TV.Brace [bodytype]
      ::xs ->
      Module (Name (s, ii1), 
              tree_list ctx bodytype)::
        tree_list ctx xs
(*
    (* todo? package ... *)
*)

    (*-------------------------------------------------------------------*)
    (* Database *)
    (*-------------------------------------------------------------------*)
(*
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

*)

    (*-------------------------------------------------------------------*)
    (* String interpolation *)
    (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* Record *)
    (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* Assign *)
    (*-------------------------------------------------------------------*)
    (* todo: yy x =, but need check on same line? *)
    |  (TV.T (T.TIdent (s0, ii0)))::
        (TV.T (T.TIdent (s1, ii1)))::(TV.T (T.TEq _))::xs ->
         VarDef (Some (type_ ctx [TV.T (T.TIdent (s0, ii0))]),
                (Name (s1, ii1)))::tree_list ctx xs

    (* x = *)
    |  (TV.T (T.TIdent (s1, ii1)))::(TV.T (T.TEq _))::xs ->
         VarDef (None, (Name(s1, ii1)))::tree_list ctx xs


    (*-------------------------------------------------------------------*)
    (* Other *)
    (*-------------------------------------------------------------------*)

    | x::xs ->
        tree ctx x::tree_list ctx xs

  (*-------------------------------------------------------------------*)
  (* Types *)
  (*-------------------------------------------------------------------*)
  and type_ ctx xs =
    match xs with
    | [(TV.T T.TIdent (s1, ii1))] -> 
        TyName ([], Name (s1, ii1))
    | [(TV.T T.TIdent (s1, ii1));TV.Paren paramstype] -> 
        TyApp (([], Name (s1, ii1)), 
               List.map (type_ ctx) paramstype)
    | xs -> 
        TyOther (tree_list ctx xs)

  and field_decl ctx xs =
    match xs with
    (* yy x *)
    | [(TV.T T.TIdent (s1, ii1));(TV.T T.TIdent (s2, ii2))] ->
        Field (Some (type_ ctx [(TV.T (T.TIdent (s1, ii1)))]),
               Name (s2, ii2))

    (* yy(zz) x *)
    | [(TV.T T.TIdent (s1, ii1));
       (TV.Paren paramstype);
       (TV.T T.TIdent (s2, ii2))] ->

        Field (Some (type_ ctx
                        [(TV.T (T.TIdent (s1, ii1)));(TV.Paren paramstype)]),
              Name (s2, ii2))
    | xs -> FieldOther (tree_list ctx xs)

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

  (*-------------------------------------------------------------------*)
  (* body *)
  (*-------------------------------------------------------------------*)
  and body ctx body =
    match body with
    | [] -> []
    | [xs] -> tree_list ctx xs
    | _::_::_ -> failwith "the body should have no comma"
  in

  tree_list top_ctx xs
