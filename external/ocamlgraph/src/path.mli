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

(* $Id: path.mli,v 1.9 2005-07-18 07:10:35 filliatr Exp $ *)

(** Paths *)

(** Minimal graph signature for Dijkstra's algorithm.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : sig
    type t
    type label
    val label : t -> label
    val src : t -> V.t
    val dst : t -> V.t
  end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
  val nb_vertex : t -> int
end

(** Signature for edges' weights. *)
module type WEIGHT = sig
  type label
    (** Type for labels of graph edges. *)
  type t
    (** Type of edges' weights. *)
  val weight : label -> t
    (** Get the weight of an edge. *)
  val compare : t -> t -> int
    (** Weights must be ordered. *)
  val add : t -> t -> t
    (** Addition of weights. *)
  val zero : t
    (** Neutral element for {!add}. *)
end

module Dijkstra
  (G: G)
  (W: WEIGHT with type label = G.E.label) :
sig

  val shortest_path : G.t -> G.V.t -> G.V.t -> G.E.t list * W.t
    (** [shortest_path g v1 v2] computes the shortest path from vertex [v1]
      to vertex [v2] in graph [g]. The path is returned as the list of
      followed edges, together with the total length of the path.
      raise [Not_found] if the path from [v1] to [v2] does not exist.

      Complexity: at most O((V+E)log(V)) *)

end

(* The following module is a contribution of Yuto Takei (University of Tokyo) *)

module BellmanFord
  (G: G)
  (W: WEIGHT with type label = G.E.label) :
sig

  module H : Hashtbl.S with type key = G.V.t

  exception NegativeCycle of G.E.t list

  val all_shortest_paths : G.t -> G.V.t -> W.t H.t
    (** [shortest_path g vs] computes the distances of shortest paths
        from vertex [vs] to all other vertices in graph [g]. They are
        returned as a hash table mapping each vertex reachable from
        [vs] to its distance from [vs].  If [g] contains a
        negative-length cycle reachable from [vs], raises
        [NegativeCycle l] where [l] is such a cycle.

        Complexity: at most O(VE) *)

  val find_negative_cycle_from: G.t -> G.V.t -> G.E.t list
    (** [find_negative_cycle_from g vs] looks for a negative-length
        cycle in graph [g] that is reachable from vertex [vs] and
        returns it as a list of edges.  If no such a cycle exists,
        raises [Not_found].

        Complexity: at most O(VE). *)

  val find_negative_cycle: G.t -> G.E.t list
    (** [find_negative_cycle g] looks for a negative-length cycle in
        graph [g] and returns it. If the graph [g] is free from such a
        cycle, raises [Not_found].

        Complexity: O(V^2E) *)
end


(** Check for a path. *)
module Check
  (G : sig
     type t
     module V : Sig.COMPARABLE
     val iter_succ : (V.t -> unit) -> t -> V.t -> unit
   end) :
sig

  type path_checker
    (** the abstract data type of a path checker; this is a mutable data
	structure *)

  val create : G.t -> path_checker
    (** [create g] builds a new path checker for the graph [g];
        if the graph is mutable, it must not be mutated while this path
        checker is in use (through the function [check_path] below). *)

  val check_path : path_checker -> G.V.t -> G.V.t -> bool
    (** [check_path pc v1 v2] checks whether there is a path from [v1] to
	[v2] in the graph associated to the path checker [pc].

        Complexity: The path checker contains a cache of all results computed
	so far. This cache is implemented with a hash table so access in this
	cache is usually O(1). When the result is not in the cache, Dijkstra's
	algorithm is run to check for the path, and all intermediate results
	are cached.

	Note: if checks are to be done for almost all pairs of vertices, it
	may be more efficient to compute the transitive closure of the graph
	(see module [Oper]).
	*)

end
