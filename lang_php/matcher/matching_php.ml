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

module Ast = Ast_php

module MV = Metavars_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module makes it possible to match one PHP AST against another PHP AST
 * providing a kind of grep but at a syntactical level.
 * 
 *)

(*****************************************************************************)
(* The functor argument *)
(*****************************************************************************)

module XMATCH = struct
  (* ------------------------------------------------------------------------*)
  (* Combinators history *) 
  (* ------------------------------------------------------------------------*)
  (*
   * version0: 
   *   type ('a, 'b) matcher = 'a -> 'b -> bool
   * 
   *   This just let you know if you matched something.
   * 
   * version1:
   *   type ('a, 'b) matcher = 'a -> 'b -> unit -> ('a, 'b) option
   * 
   *   The Maybe monad.
   * 
   * version2:
   *   type ('a, 'b) matcher = 'a -> 'b -> binding -> binding list
   * 
   *   Why not returning a binding option ? because I may need at some
   *   point to return multiple possible bindings for one matching code.
   *   For instance with the pattern do 'f(..., X, ...)', X could be binded
   *   to different parts of the code.
   *   Note that the empty list means a match failure.
   *)

  type tin = MV.metavars_binding
  type 'x tout = ('x * MV.metavars_binding) list
  type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

  let ((>>=):
          (tin -> ('a * 'b) tout)  -> 
          (('a * 'b) -> (tin -> ('c * 'd) tout)) -> 
          (tin -> ('c * 'd) tout)) = 
    fun m1 m2 ->
      fun tin ->
        (* old:
           match m1 tin with
           | None -> None
           | Some (a,b) ->
           m2 (a, b) tin
        *)
        (* let's get a list of possible environment match (could be 
         * the empty list when it didn't match, playing the role None
         * had before)
         *)
        let xs = m1 tin in
        (* try m2 on each possible returned bindings *)
        let xxs = xs +> List.map (fun ((a,b), binding) -> 
          m2 (a, b) binding
        ) in
        List.flatten xxs


  let (>||>) m1 m2 = fun tin ->
(* CHOICE
      let xs = m1 tin in
      if null xs
      then m2 tin
      else xs
*)
    (* opti? use set instead of list *)
    m1 tin ++ m2 tin

           
  let return (a,b) = fun tin ->
    (* old: Some (a,b) *)
    [(a,b), tin]
      
  let fail = fun tin ->
    (* old: None *)
    []

  (* ------------------------------------------------------------------------*)
  (* Environment *) 
  (* ------------------------------------------------------------------------*)

  (* pre: both 'a' and 'b' contains only regular PHP code. There is no
   * metavariables in them.
   *)
  let equal_ast_binded_code a b =
    match a, b with
    | Ast.Expr a, Ast.Expr b ->

        (* Note that because we want to retain the position information
         * of the matched code in the environment (e.g. for the -pvar
         * sgrep command line argument), we can not just use the
         * generic '=' OCaml operator as 'a' and 'b' may represent
         * the same code but they will contain leaves in their AST
         * with different position information. So before doing
         * the comparison we just need to remove/abstract-away 
         * the line number information in each ASTs.
         * 
         * todo: optimize by caching the abstract_lined ?
         *)
        let a = Lib_parsing_php.abstract_position_info_expr a in
        let b = Lib_parsing_php.abstract_position_info_expr b in
        a = b
    | _, _ -> 
        false

  let check_and_add_metavar_binding  (mvar, valu) = fun tin ->
    match Common.assoc_option mvar tin with
    | Some valu' ->
        (* TODO: have to ensure both matched ASTs are equal, valu =? valu'.
         *
         * Should we use php_vs_php itself for comparing the binded code ?
         * Hmmm, we can't because it leads to a circular dependencies.
         * Moreover here we know both valu and valu' are regular PHP code,
         * not PHP patterns, so we can just use the generic '=' of OCaml.
        *)
        if equal_ast_binded_code valu valu'
        then Some tin
        else None
    | None ->
        (* first time the metavar is binded. Just add it to the environment *)
        Some (Common.insert_assoc (mvar, valu) tin)

  let (envf: (Metavars_php.mvar Ast_php.wrap, Ast_php.any) matcher) =
   fun (mvar, imvar) any  -> fun tin ->
    match check_and_add_metavar_binding (mvar, any) tin with
    | None ->
        fail tin
    | Some new_binding ->
        return ((mvar, imvar), any) new_binding

  let tokenf a b = 
    (* dont care about position, space/indent/comment isomorphism *)
    return (a, b)
    
end

(*****************************************************************************)
(* Entry point  *) 
(*****************************************************************************)

module MATCH  = Php_vs_php.PHP_VS_PHP (XMATCH)

type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_php.metavars_binding list

let (extract_bindings: 'a XMATCH.tout -> MV.metavars_binding list) = fun tout ->
  tout +> List.map (fun (term, env) -> env)
  

let match_e_e pattern e = 
  let env = MV.empty_environment in
  MATCH.m_expr pattern e env +> extract_bindings


let match_v_v pattern e = 
  let env = MV.empty_environment in
  MATCH.m_variable pattern e env +> extract_bindings
  
