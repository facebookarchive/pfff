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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
    | Method def when 
          Ast.name def.m_name = constructor_name ->
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
