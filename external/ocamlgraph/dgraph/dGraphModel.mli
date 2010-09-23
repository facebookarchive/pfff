(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

(** Abstract graph model *)

open XDot
open Ocamlgraph

exception DotError of string

(** Immutable graph model.
    Layout accessors, iterators and
    membership functions. *)
class type ['vertex, 'edge, 'cluster] abstract_model = object
  method iter_edges : ('vertex -> 'vertex -> unit) -> unit
  method iter_edges_e : ('edge -> unit) -> unit
  method iter_pred : ('vertex -> unit) -> 'vertex -> unit
  method iter_pred_e : ('edge -> unit) -> 'vertex -> unit
  method iter_succ : ('vertex -> unit) -> 'vertex -> unit
  method iter_succ_e : ('edge -> unit) -> 'vertex -> unit
  method iter_vertex : ('vertex -> unit) -> unit
  method iter_clusters : ('cluster -> unit) -> unit

  (** Membership functions *)
  method find_edge : 'vertex -> 'vertex -> 'edge
  method mem_edge : 'vertex -> 'vertex -> bool
  method mem_edge_e : 'edge -> bool
  method mem_vertex : 'vertex -> bool
  method src : 'edge -> 'vertex
  method dst : 'edge -> 'vertex

  (** Dot layout *)
  method bounding_box : bounding_box
  method get_edge_layout : 'edge -> edge_layout
  method get_vertex_layout : 'vertex -> node_layout
  method get_cluster_layout : 'cluster -> cluster_layout
end

(** This functor creates a model from a graph *)
module Make(G : Ocamlgraph.Graphviz.GraphWithDotAttrs) : sig

  open G

  type cluster = string

  class model :
    (G.vertex, G.edge, cluster) XDot.graph_layout -> G.t -> 
    [G.vertex, G.edge, cluster] abstract_model

      (** Creates a model using graphviz.
	  [tmp_name] is the name of the temporary dot files *)
  val from_graph : ?cmd:string -> ?tmp_name:string -> G.t -> model

end


module Vertex : Sig.ANY_TYPE with type t = XDot.node_layout
module Edge : Sig.ORDERED_TYPE_DFT with type t = XDot.edge_layout
module DotG : 
  Sig.G with type t = Ocamlgraph.Imperative.Digraph.AbstractLabeled(Vertex)(Edge).t
type cluster = string
type dotg_model = (DotG.vertex, DotG.edge, cluster) abstract_model

(** Creates a model from a dot file *)
val read_dot : ?cmd:string -> dot_file:string -> dotg_model

(** Creates a model from an xdot file (the layout is not recomputed)*)
val read_xdot : xdot_file:string -> dotg_model
