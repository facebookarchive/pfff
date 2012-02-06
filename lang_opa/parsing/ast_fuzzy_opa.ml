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

let vof_token t =
  Ocaml.VString (Token_helpers_opa.str_of_tok t)

let rec vof_tree =
  function

  | T v1 -> let v1 = vof_token v1 in Ocaml.VSum (("T", [ v1 ]))
  | Paren v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Paren", [ v1 ]))
  | Brace v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Brace", [ v1 ]))
  | Bracket v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Bracket", [ v1 ]))
  | Xml ((v1, v2)) ->
      let v1 = Ocaml.vof_list vof_tree v1
      and v2 = Ocaml.vof_list vof_tree v2
      in Ocaml.VSum (("Xml", [ v1; v2 ]))
  | _ -> raise Todo
  
let vof_tree_list xs = Ocaml.vof_list vof_tree xs

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

    (* function x(...) { ... } *)
    |   (TV.T T.Tfunction _)
      ::(TV.T T.TIdent (s1, ii1))
      ::(TV.Paren params)
      ::(TV.Brace bdy)
      ::xs ->
        let params = List.map (parameter ctx) params in
        let bdy = body ctx bdy in
        Function ({
          f_name = Some (Name (s1, ii1));
          f_ret_type = None;
          f_params = params;
          f_body = bdy;
        })::tree_list ctx xs

(*
        tag ii1 (Function (Def2 fake_no_def2));
        List.iter (tree_tree InParameter) params;
        tree_tree InFunction [(TV.Brace body)];
        tree_tree ctx xs
*)

    | x::xs ->
        tree ctx x::tree_list ctx xs

  and parameter ctx param = 
    raise Todo
  and body ctx body =
    raise Todo
  in


  tree_list top_ctx xs
