(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*
Copyright © 2009 Carnegie-Mellon University, David Brumley, and Ivan Jager.
From the BAP library; see http://bap.ece.cmu.edu
*)

(** Dominators

    All of the functions in this module assume that the graph is not modified
    between calling one of these functions and using the returned functions.
    Such mutation results in undefined behavior.
    @author Ivan Jager
*)

exception Unreachable

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val pred : t -> V.t -> V.t list
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_vertex : (V.t -> unit) -> t -> unit
  val nb_vertex : t -> int
  val create: ?size:int -> unit -> t
  val add_edge : t -> V.t -> V.t -> unit
end

module Make(G : G) : sig
  module S : Set.S with type elt = G.V.t

  (** function from [n] to [n]'s immediate dominator *)
  type idom = G.V.t -> G.V.t

  (** [idoms x y] is true when [x] is [y]'s immediate dominator *)
  type idoms = G.V.t -> G.V.t -> bool

  (** function from [x] to a list of nodes immediately dominated by [x] *)
  type dom_tree = G.V.t -> G.V.t list

  (** function from node to a list of nodes that dominate it. *)
  type dominators = G.V.t -> G.V.t list

  (** [dom x y] returns true iff [x] dominates [y] *)
  type dom = G.V.t -> G.V.t -> bool

  (** [sdom x y] returns true iff [x] strictly dominates [y]. *)
  type sdom = G.V.t -> G.V.t -> bool

  (** function from [x] to a list of nodes not dominated by [x], but with
      predecessors which are dominated by [x] *)
  type dom_frontier = G.V.t -> G.V.t list

  type dom_graph = unit -> G.t

  type dom_functions = {
    idom : idom;
    idoms : idoms;
    dom_tree : dom_tree;
    dominators : dominators;
    dom : dom;
    sdom : sdom;
    dom_frontier : dom_frontier;
    dom_graph : dom_graph
  }

  (** Computes the dominator tree, using the Lengauer-Tarjan algorithm.
      [compute_idom cfg s0] returns a function [idom : V.t -> V.t] s.t.
      [idom x] returns the immediate dominator of [x]
  *)
  val compute_idom : G.t -> G.V.t -> G.V.t -> G.V.t

  (** Given a function from a node to it's dominators, returns a function
      [dom : V.t -> V.t -> bool] s.t. [dom x y] returns true when
      [x] dominates [y]
  *)
  val dominators_to_dom : ('a -> S.t) -> S.elt -> 'a -> bool

  (** Given a function from a node to it's dominators, returns a function
      [sdom : V.t -> V.t -> bool] s.t. [sdom x y] returns true when
      [x] strictly dominates [y]
  *)
  val dominators_to_sdom : (G.V.t -> S.t) -> S.elt -> G.V.t -> bool
  val dom_to_sdom : (G.V.t -> G.V.t -> bool) -> G.V.t -> G.V.t -> bool

  (** Given a a function from a node to it's dominators, returns a function
      from a node to it's strict dominators. *)
  val dominators_to_sdominators : (S.elt -> S.t) -> S.elt -> S.t

  (** Given a function from a node to it's dominators, returns a function
      [idoms : G.V.t -> G.V.t -> bool] s.t. [idoms x y] returns true when
      [x] is the immediate dominator of [y].
  *)
  val dominators_to_idoms : (S.elt -> S.t) -> S.elt -> S.elt -> bool

  (** Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and dominator function.
      Note: The dominator tree is also called [IDom] by Muchnick.
      Note: If you are computing a post-dominator tree, then the
      optional argument pred should be G.succ.
  *)
  val dominators_to_dom_tree :
    G.t ->
    ?pred:(G.t -> S.elt -> S.elt list) -> (S.elt -> S.t) -> S.elt -> S.t

  (** Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and idom function.
  *)
  val idom_to_dom_tree : G.t -> (G.V.t -> G.V.t) -> G.V.t -> G.V.t list

  val idom_to_idoms : idom -> G.V.t -> G.V.t -> bool

  (** Computes the dominance frontier.
      As specified in section 19.1 of Modern Compiler Implementation in ML
      by Andrew Appel.
  *)
  val compute_dom_frontier :
    G.t -> dom_tree -> idom -> G.V.t -> G.V.t list

  val idom_to_dominators : ('a -> 'a) -> 'a -> 'a list

  val idom_to_dom : (G.V.t -> G.V.t) -> G.V.t -> G.V.t -> bool

  val compute_dom_graph : G.t -> dom_tree -> G.t

  (** Computes all dominance functions.

      This function computes some things eagerly and some lazily, so don't
      worry about it doing extra work to compute functions you don't need,
      but also don't call it if you aren't going to use anything it returns.

      @return a record containing all dominance functions for the given graph
      and entry node.
  *)
  val compute_all : G.t -> G.V.t -> dom_functions
end
