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

type tree =

  | TreeTodo
  (* a copy of Token_views_opa.tree *)
  | T of Parser_opa.token
  | Paren of tree list list
  | Brace of tree list list
  | Bracket of tree list list
  | Xml of tree list * tree list

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
  let rec aux ctx = function

  | TV.T tok -> T tok
  | TV.Paren xxs ->
      let xxs = xxs +> List.map (aux_list ctx) in
      Paren xxs
  | TV.Brace xxs ->
      let xxs = xxs +> List.map (aux_list ctx) in
      Brace xxs
  | TV.Bracket xxs ->
      let xxs = xxs +> List.map (aux_list ctx) in
      Bracket xxs
  | TV.Xml ((v1, v2)) ->
      raise Todo

  and aux_list ctx xs = 

    match xs with
    | [] -> []
    | x::xs ->
        aux ctx x::aux_list ctx xs
  in

  aux_list top_ctx xs
