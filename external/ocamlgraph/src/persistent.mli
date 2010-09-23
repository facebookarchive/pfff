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

(* $Id: persistent.mli,v 1.13 2006-05-12 14:07:16 filliatr Exp $ *)

(** Persistent Graph Implementations. *)

open Sig

(** Signature of persistent graphs. *)
module type S = sig

  (** <b>Edges may be labeled or not</b>:
      - Unlabeled: there is no label on edges
      - Labeled: you have to provide a label implementation as a functor
      parameter.

      <b>Vertices may be concrete or abstract</b>:
      - Concrete: type of vertex labels and type of vertices are identified.
      - Abstract: type of vertices is abstract (in particular it is not equal
      to type of vertex labels

      <b>How to choose between concrete and abstract vertices for my graph 
      implementation</b>?

      Usually, if you fall into one of the following cases, use abstract
      vertices: 
      - you cannot provide efficient comparison/hash functions for vertices; or
      - you wish to get two different vertices with the same label.

      In other cases, it is certainly easier to use concrete vertices.  *)

  (** Persistent Unlabeled Graphs. *)
  module Concrete (V: COMPARABLE) : 
    Sig.P with type V.t = V.t and type V.label = V.t and type E.t = V.t * V.t
	  and type E.label = unit

  (** Abstract Persistent Unlabeled Graphs. *)
  module Abstract(V: ANY_TYPE) : Sig.P with type V.label = V.t 
				       and type E.label = unit

  (** Persistent Labeled Graphs. *)
  module ConcreteLabeled (V: COMPARABLE)(E: ORDERED_TYPE_DFT) :
    Sig.P with type V.t = V.t and type V.label = V.t
	    and type E.t = V.t * E.t * V.t and type E.label = E.t

  (** Abstract Persistent Labeled Graphs. *)
  module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
    Sig.P with type V.label = V.t and type E.label = E.t

end

(** Persistent Directed Graphs. *)
module Digraph : S

(** Persistent Undirected Graphs. *)
module Graph : S
