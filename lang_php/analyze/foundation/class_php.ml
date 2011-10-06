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

open Ast_php

module Ast = Ast_php
module V = Visitor_php
module E = Entity_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This is ugly. Some of the code requires to have a 'name' type
 * for every "entities" we are defining and checking. For a class 
 * constant we should really have a pair of name, one for the class
 * and one for the constant itself. Instead we abuse 'name' and
 * pack into it also the classname.
 *)

(*
let rewrap_name_with_class_name classname name =
  match name with 
  | Name (s, info) ->
      let new_str = spf "%s::%s" classname s in
      Name (new_str, Ast.rewrap_str new_str info)
  (* only classnames can be a XhpName. Constants (or functions)
   * are always simple names
   *)
  | XhpName _ ->
      failwith "Impossible: only classes can be XhpName"

let mk_class_name s info = 
  Name (s, info)

let resolve_class_name qu =
  match fst qu with
  | ClassName (name) -> Some name
  | Self _ | Parent _ -> 
      pr2_once "check_functions_php: call unsugar_self_parent";
      None
  | LateStatic _ ->
      pr2 "late static: can't resolve, add a pattern match for LateStatic";
      None
*)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let constructor_name = "__construct"

(*****************************************************************************)
(* Ast Helpers *)
(*****************************************************************************)

(* This is used in check_variables_php.ml to allow inherited
 * visible variables to be used in scope
 *)
let get_public_or_protected_vars_of_class def =

  def.c_body +> Ast.unbrace +> Common.map_filter (function
  |  ClassVariables (modifiers, _opt_ty, class_vars, _tok) ->
       
       let modifiers = Ast.unmodifiers modifiers in
       
       if List.mem Public modifiers ||
          List.mem Protected modifiers
       then
         let dnames = 
           class_vars |> Ast.uncomma |> List.map fst
         in
         Some dnames
       else None

  | (XhpDecl _|Method _|ClassConstants (_, _, _)) ->
      (* could maybe do something with XhpDecl ? *)
      None
  ) +> List.flatten


(* TODO: it could also be one which has the same name than the class.
 *  print a warning to tell to use __construct instead ?
 *)
let get_constructor def =
  def.c_body +> Ast.unbrace +> Common.find_some (fun class_stmt ->
    match class_stmt with
    | Method def when Ast.name def.m_name = constructor_name ->
        Some def
    | _ -> None
  )

(* This is useful when one needs to add class variables in scope.
 * Because they may be at the end and that simple algorithm are just
 * one pass on the ast, just simple to reorder the variables so that
 * they are first. See Check_variables_php.
 *)
let class_variables_reorder_first def = 
  let (lb, body, rb) = def.c_body in
  let body' =
    let (vars, rest) = 
      body +> List.partition (function
      | ClassVariables _ -> true
      | _ -> false
      )
    in
    vars ++ rest
  in
  { def with
    c_body = (lb, body', rb);
  }

let is_static_method def =
  let modifiers = def.m_modifiers +> List.map Ast.unwrap in
  List.mem Ast.Static modifiers


(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)

(* todo: for privacy aware lookup it will require to give some context
 * about where is coming from the lookup, from the class itself ?
 *)
let lookup_method ?(case_insensitive=false) (aclass, amethod) find_entity =
  let equal a b = 
    if case_insensitive 
    then String.lowercase a =$= String.lowercase b
    else a =$= b
  in
  let rec aux aclass =
    match find_entity (E.Class, aclass) with
    | [ClassE def] ->
        (try 
          def.c_body +> Ast.unbrace +> Common.find_some (function
            | Method def when equal (Ast.name def.m_name) amethod -> Some def
            | _ -> None
          )
        with Not_found ->
          (match def.c_extends with
          | None -> raise Not_found
          | Some (_, name) ->
              let str = Ast.name name in
              (* recurse *)
              aux str
          )
        )
    | [] -> raise Not_found
    | x::y::xs -> raise Multi_found
    | [_] -> raise Impossible
  in
  aux aclass
