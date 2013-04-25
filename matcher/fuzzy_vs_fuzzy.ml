(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

(* A is the pattern, and B the concrete source code. For now
 * we both use the same module, Ast_fuzzy, but they may differ later
 * as the expressivity of the pattern language grows.
 *)
module A = Ast_fuzzy
module B = Ast_fuzzy

module MV = Metavars_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module provides a big functor, X_VS_X, which can be used
 * to match some Ast_fuzzy trees against other Ast_fuzzy trees in
 * a flexible way.
 *)


(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Functor parameter combinators *)
(*****************************************************************************)

(* This is the interface of the structure that will be passed to the
 * X_VS_X functor below.
 *)
module type PARAM =
  sig
    (* tin is for 'type in' and tout for 'type out' *)
    type tin
    type 'x tout

    (* A matcher is something taking an element A and an element B
     * (for X_VS_X below, A will be the AST of the pattern and B
     * the AST of the program we want to match over), then some environment
     * information tin, and it will return something (tout) that will
     * encapsulate the possible matched element A and B.
     *
     * If you just want to do a simple matcher that just returns
     * a boolean, then instantiate the PARAM struct with
     *   type tin = unit  (* no environment information *)
     *   type ('a * 'b) tout = ('a * 'b) option
     * and if the toplevel matching returns a None, then you know
     * A didn't match B.
     *)
    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

    (* The >>= combinator below allow you to configure the matching process
     * anyway you want. Essentially this combinator takes a matcher,
     * another matcher, and returns a matcher that combine the 2
     * matcher arguments.
     *
     * In the case of a simple boolean matcher, you just need to write:
     *
     *   let (>>=) m1 m2 = fun tin ->
     *    match m1 tin with
     *    | None -> None
     *    | Some (a,b) ->
     *        m2 (a, b) tin
     *)
    val (>>=):
      (tin -> ('a * 'b) tout)  ->
      ('a * 'b -> (tin -> ('c * 'd) tout)) ->
      (tin -> ('c * 'd) tout)


    (* the disjunctive combinator *)
    val (>||>) :
      (tin -> 'x tout) ->
      (tin -> 'x tout) ->
      (tin -> 'x tout)


    (* The classical monad combinators *)
    val return : ('a * 'b) -> tin -> ('a *'b) tout
    val fail : tin -> ('a * 'b) tout

(*
    val tokenf : (A.info, B.info) matcher

    val envf : (Metavars_php.mvar Ast_php.wrap, Ast_php.any) matcher
    (* ugly hack for the "A" string metavariables *)
    val envf2 : (Metavars_php.mvar Ast_php.wrap, Ast_php.any * Ast_php.any) matcher
*)

  end

(*****************************************************************************)
(* Functor code, "X vs X" *)
(*****************************************************************************)

module X_VS_X =
  functor (X : PARAM) ->
struct

type ('a, 'b) matcher = 'a -> 'b  -> X.tin -> ('a * 'b) X.tout

let (>>=) = X.(>>=)
let (>||>) = X.(>||>)

let return =
  X.return
let fail () =
  X.fail

(* ---------------------------------------------------------------------- *)
(* option, list, ref, either *)
(* ---------------------------------------------------------------------- *)

let rec m_list f a b =
  match a, b with
  | [], [] ->
      return ([], [])
  | xa::aas, xb::bbs ->
      f xa xb >>= (fun (xa, xb) ->
      m_list f aas bbs >>= (fun (aas, bbs) ->
        return (
          xa::aas,
          xb::bbs
        )
      )
      )
  | [], _
  | _::_, _ ->
      fail ()

(* ---------------------------------------------------------------------- *)
(* m_string *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* tokens *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* trees *)
(* ---------------------------------------------------------------------- *)
let m_trees a b =
  raise Todo

end
