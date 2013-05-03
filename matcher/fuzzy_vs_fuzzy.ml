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

    (* The >>= combinator below allows you to configure the matching process
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

    val tokenf : (Parse_info.info, Parse_info.info) matcher

(*

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
(* tokens *)
(* ---------------------------------------------------------------------- *)
let m_tok a b =
  X.tokenf a b

(* ---------------------------------------------------------------------- *)
(* list of trees *)
(* ---------------------------------------------------------------------- *)
let rec m_list__m_tree xsa xsb =
    match xsa, xsb with
  | [], [] ->
      return ([], [])

  (* iso: allow "..." to match any list of trees *)
  | [A.Tok t],  bbs    when Parse_info.str_of_info t =$= "..." ->
    (* todo: check no annot on t *)
      return (
        xsa,
        xsb
      )

  | xa::aas, xb::bbs ->
      m_tree xa xb >>= (fun (xa, xb) ->
      m_list__m_tree aas bbs >>= (fun (aas, bbs) ->
        return (
          xa::aas,
          xb::bbs
        )
      )
      )
  | [], _::_
  | _::_, _ ->
      fail ()


(* ---------------------------------------------------------------------- *)
(* tree *)
(* ---------------------------------------------------------------------- *)
and m_tree a b =
  match a, b with
  | A.Braces (a1, a2, a3), B.Braces (b1, b2, b3) ->
    m_tok a1 b1 >>= (fun (a1, b1) ->
    m_trees a2 b2 >>= (fun (a2, b2) ->
    m_tok a3 b3 >>= (fun (a3, b3) ->
      return (
        A.Braces (a1, a2, a3), 
        B.Braces (b1, b2, b3)
      )
    )))
  | A.Parens (a1, a2, a3), B.Parens (b1, b2, b3) ->
    m_tok a1 b1 >>= (fun (a1, b1) ->
    m_trees a2 b2 >>= (fun (a2, b2) ->
    m_tok a3 b3 >>= (fun (a3, b3) ->
      return (
        A.Parens (a1, a2, a3), 
        B.Parens (b1, b2, b3)
      )
    )))
  | A.Angle (a1, a2, a3), B.Angle (b1, b2, b3) ->
    m_tok a1 b1 >>= (fun (a1, b1) ->
    m_trees a2 b2 >>= (fun (a2, b2) ->
    m_tok a3 b3 >>= (fun (a3, b3) ->
      return (
        A.Angle (a1, a2, a3), 
        B.Angle (b1, b2, b3)
      )
    )))
  | A.Tok a1, B.Tok b1 ->
    m_tok a1 b1 >>= (fun (a1, b1) ->
      return (
        A.Tok a1,
        B.Tok b1
      )
    )

  | A.Braces _, _
  | A.Parens _, _
  | A.Angle _, _
  | A.Tok _, _
    -> fail ()

and m_trees a b = m_list__m_tree a b

end
