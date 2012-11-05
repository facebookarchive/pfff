(*
 * Copyright (C) 2008 Jean-Christophe Filliatre
 * Copyright (C) 2008, 2009 Laurent Hubert (CNRS)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.

 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)

(** Sets of integers implemented as Patricia trees.

    This implementation follows Chris Okasaki and Andrew Gill's paper
    {e Fast Mergeable Integer Maps}.

    Patricia trees provide faster operations than standard library's
    module [Set], and especially very fast [union], [subset], [inter]
    and [diff] operations. *)

(** The idea behind Patricia trees is to build a {e trie} on the
    binary digits of the elements, and to compact the representation
    by branching only one the relevant bits (i.e. the ones for which
    there is at least on element in each subtree). We implement here
    {e little-endian} Patricia trees: bits are processed from
    least-significant to most-significant. The trie is implemented by
    the following type [t]. [Empty] stands for the empty trie, and
    [Leaf k] for the singleton [k]. (Note that [k] is the actual
    element.) [Branch (m,p,l,r)] represents a branching, where [p] is
    the prefix (from the root of the trie) and [m] is the branching
    bit (a power of 2). [l] and [r] contain the subsets for which the
    branching bit is respectively 0 and 1. Invariant: the trees [l]
    and [r] are not empty. *)

(** The docuemntation is given for function that differs from [Set.S
    with type elt = int]. *)

module type S = sig
  type t

  type elt = int

  val empty : t

  val is_empty : t -> bool

  val mem : int -> t -> bool

  val add : int -> t -> t

  val singleton : int -> t

  val remove : int -> t -> t

  val union : t -> t -> t

  val subset : t -> t -> bool

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val elements : t -> int list

  val choose : t -> int

  val cardinal : t -> int

  val iter : (int -> unit) -> t -> unit

  val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (int -> bool) -> t -> bool

  val exists : (int -> bool) -> t -> bool

  val filter : (int -> bool) -> t -> t

  val partition : (int -> bool) -> t -> t * t

  val split : int -> t -> t * bool * t

  (** Warning: [min_elt] and [max_elt] are linear w.r.t. the size of the
      set. In other words, [min_elt t] is barely more efficient than [fold
      min t (choose t)]. *)

  val min_elt : t -> int
  val max_elt : t -> int

  (** Additional functions not appearing in the signature [Set.S] from
      ocaml standard library. *)

  (** [intersect u v] determines if sets [u] and [v] have a non-empty
      intersection. *)

  val intersect : t -> t -> bool

  (** [choose_and_remove t] is equivalent (but more efficient) to
      [(fun t -> let i = choose t in (i,remove i t)) t]
      @author Laurent Hubert*)
  val choose_and_remove : t -> int*t

  (** [to_string s] return a string representation of [s]
      @author David Pichardie *)
  val to_string : t -> string

end

include S

(** Big-endian Patricia trees *)

module Big : S

(** Big-endian Patricia trees with non-negative elements. Changes:
    - [add] and [singleton] raise [Invalid_arg] if a negative element is given
    - [mem] is slightly faster (the Patricia tree is now a search tree)
    - [min_elt] and [max_elt] are now O(log(N))
    - [elements] returns a list with elements in ascending order
 *)

module BigPos : S


