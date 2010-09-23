(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2008                                               *)
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

(* $Id: topological.mli,v 1.7 2004-12-20 13:35:42 filliatr Exp $ *)

(** Topological order.

    This functor provides functions which allow iterating over a graph in
    topological order. *)

(** Minimal graph signature to provide.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.HASHABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val in_degree : t -> V.t -> int
end

(** Functor providing topological iterators over a graph. *)
module Make(G: G) : sig

  val fold : (G.V.t -> 'a -> 'a) -> G.t -> 'a -> 'a
    (** [fold action g seed] allows iterating over the graph [g] 
      in topological order. [action node accu] is called repeatedly,
      where [node] is the node being visited, and [accu] is the result of 
      the [action]'s previous invocation, if any, and [seed] otherwise. 
      If [g] contains cycles, the order is unspecified inside the cycles and 
      every node in the cycles will be presented exactly once. *)

  val iter : (G.V.t -> unit) -> G.t -> unit
    (** [iter action] calls [action node] repeatedly. Nodes are (again) 
        presented to [action] in topological order.
        The order is the same as for [fold]. *)

end


