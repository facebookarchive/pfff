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


(*s Sets of integers implemented as Patricia trees, following Chris
    Okasaki and Andrew Gill's paper {\em Fast Mergeable Integer Maps}
    ({\tt\small http://www.cs.columbia.edu/\~{}cdo/papers.html\#ml98maps}).
    Patricia trees provide faster operations than standard library's
    module [Set], and especially very fast [union], [subset], [inter]
    and [diff] operations. *)

(*s The idea behind Patricia trees is to build a {\em trie} on the
    binary digits of the elements, and to compact the representation
    by branching only one the relevant bits (i.e. the ones for which
    there is at least on element in each subtree). We implement here
    {\em little-endian} Patricia trees: bits are processed from
    least-significant to most-significant. The trie is implemented by
    the following type [t]. [Empty] stands for the empty trie, and
    [Leaf k] for the singleton [k]. (Note that [k] is the actual
    element.) [Branch (m,p,l,r)] represents a branching, where [p] is
    the prefix (from the root of the trie) and [m] is the branching
    bit (a power of 2). [l] and [r] contain the subsets for which the
    branching bit is respectively 0 and 1. Invariant: the trees [l]
    and [r] are not empty. *)


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
  val min_elt : t -> int
  val max_elt : t -> int
  val intersect : t -> t -> bool
  val choose_and_remove : t -> int*t
  val to_string : t -> string
end


(*i*)
type elt = int
(*i*)

type t =
  | Empty
  | Leaf of int
  | Branch of int * int * t * t

(*s Example: the representation of the set $\{1,4,5\}$ is
    $$\mathtt{Branch~(0,~1,~Leaf~4,~Branch~(1,~4,~Leaf~1,~Leaf~5))}$$
    The first branching bit is the bit 0 (and the corresponding prefix
    is [0b0], not of use here), with $\{4\}$ on the left and $\{1,5\}$ on the
    right. Then the right subtree branches on bit 2 (and so has a branching
    value of $2^2 = 4$), with prefix [0b01 = 1]. *)

(*s Empty set and singletons. *)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton k = Leaf k

(*s Testing the occurrence of a value is similar to the search in a
    binary search tree, where the branching bit is used to select the
    appropriate subtree. *)

let zero_bit k m = (k land m) == 0

let rec mem k = function
  | Empty -> false
  | Leaf j -> k == j
  | Branch (_, m, l, r) -> mem k (if zero_bit k m then l else r)

(*s The following operation [join] will be used in both insertion and
    union. Given two non-empty trees [t0] and [t1] with longest common
    prefixes [p0] and [p1] respectively, which are supposed to
    disagree, it creates the union of [t0] and [t1]. For this, it
    computes the first bit [m] where [p0] and [p1] disagree and create
    a branching node on that bit. Depending on the value of that bit
    in [p0], [t0] will be the left subtree and [t1] the right one, or
    the converse. Computing the first branching bit of [p0] and [p1]
    uses a nice property of twos-complement representation of integers. *)

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
  if zero_bit p0 m then
    Branch (mask p0 m, m, t0, t1)
  else
    Branch (mask p0 m, m, t1, t0)

(*s Then the insertion of value [k] in set [t] is easily implemented
    using [join].  Insertion in a singleton is just the identity or a
    call to [join], depending on the value of [k].  When inserting in
    a branching tree, we first check if the value to insert [k]
    matches the prefix [p]: if not, [join] will take care of creating
    the above branching; if so, we just insert [k] in the appropriate
    subtree, depending of the branching bit. *)

let match_prefix k p m = (mask k m) == p

let add k t =
  let rec ins = function
    | Empty -> Leaf k
    | Leaf j as t ->
	if j == k then t else join (k, Leaf k, j, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k, Leaf k, p, t)
  in
  ins t

(*s The code to remove an element is basically similar to the code of
    insertion. But since we have to maintain the invariant that both
    subtrees of a [Branch] node are non-empty, we use here the
    ``smart constructor'' [branch] instead of [Branch]. *)

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let rec rmv = function
    | Empty -> Empty
    | Leaf j as t -> if k == j then Empty else t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in
  rmv t

let rec choose_and_remove = function
  | Empty -> raise Not_found
  | Leaf j -> (j,Empty)
  | Branch (p,m,t0,t1) ->
      let (j,t0') = choose_and_remove t0
      in (j, branch (p,m,t0',t1))

(*s One nice property of Patricia trees is to support a fast union
    operation (and also fast subset, difference and intersection
    operations). When merging two branching trees we examine the
    following four cases: (1) the trees have exactly the same
    prefix; (2/3) one prefix contains the other one; and (4) the
    prefixes disagree. In cases (1), (2) and (3) the recursion is
    immediate; in case (4) the function [join] creates the appropriate
    branching. *)

let rec merge = function
  | t1,t2 when t1==t2 -> t1
  | Empty, t  -> t
  | t, Empty  -> t
  | Leaf k, t -> add k t
  | t, Leaf k -> add k t
  | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
      if m == n && match_prefix q p m then
	(* The trees have the same prefix. Merge the subtrees. *)
	Branch (p, m, merge (s0,t0), merge (s1,t1))
      else if m < n && match_prefix q p m then
	(* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	if zero_bit q m then
	  Branch (p, m, merge (s0,t), s1)
        else
	  Branch (p, m, s0, merge (s1,t))
      else if m > n && match_prefix p q n then
	(* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	if zero_bit p n then
	  Branch (q, n, merge (s,t0), t1)
	else
	  Branch (q, n, t0, merge (s,t1))
      else
	(* The prefixes disagree. *)
	join (p, s, q, t)

let union s t = merge (s,t)

(*s When checking if [s1] is a subset of [s2] only two of the above
    four cases are relevant: when the prefixes are the same and when the
    prefix of [s1] contains the one of [s2], and then the recursion is
    obvious. In the other two cases, the result is [false]. *)

let rec subset s1 s2 = match (s1,s2) with
  | Empty, _ -> true
  | _, Empty -> false
  | Leaf k1, _ -> mem k1 s2
  | Branch _, Leaf _ -> false
  | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
      if m1 == m2 && p1 == p2 then
	subset l1 l2 && subset r1 r2
      else if m1 > m2 && match_prefix p1 p2 m2 then
	if zero_bit p1 m2 then
	  subset l1 l2 && subset r1 l2
	else
	  subset l1 r2 && subset r1 r2
      else
	false

(*s To compute the intersection and the difference of two sets, we
    still examine the same four cases as in [merge]. The recursion is
    then obvious. *)

let rec inter s1 s2 = match (s1,s2) with
  | t1, t2 when t1==t2 -> t1
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Leaf k1, _ -> if mem k1 s2 then s1 else Empty
  | _, Leaf k2 -> if mem k2 s1 then s2 else Empty
  | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
      if m1 == m2 && p1 == p2 then
	merge (inter l1 l2, inter r1 r2)
      else if m1 < m2 && match_prefix p2 p1 m1 then
	inter (if zero_bit p2 m1 then l1 else r1) s2
      else if m1 > m2 && match_prefix p1 p2 m2 then
	inter s1 (if zero_bit p1 m2 then l2 else r2)
      else
	Empty

let rec diff s1 s2 = match (s1,s2) with
  | Empty, _ -> Empty
  | _, Empty -> s1
  | Leaf k1, _ -> if mem k1 s2 then Empty else s1
  | _, Leaf k2 -> remove k2 s1
  | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
      if m1 == m2 && p1 == p2 then
	merge (diff l1 l2, diff r1 r2)
      else if m1 < m2 && match_prefix p2 p1 m1 then
	if zero_bit p2 m1 then
	  merge (diff l1 s2, r1)
	else
	  merge (l1, diff r1 s2)
      else if m1 > m2 && match_prefix p1 p2 m2 then
	if zero_bit p1 m2 then diff s1 l2 else diff s1 r2
      else
	s1

(*s All the following operations ([cardinal], [iter], [fold], [for_all],
    [exists], [filter], [partition], [choose], [elements]) are
    implemented as for any other kind of binary trees. *)

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

let rec iter f = function
  | Empty -> ()
  | Leaf k -> f k
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf k -> f k accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

let rec for_all p = function
  | Empty -> true
  | Leaf k -> p k
  | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

let rec exists p = function
  | Empty -> false
  | Leaf k -> p k
  | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

let rec filter pr = function
  | Empty -> Empty
  | Leaf k as t -> if pr k then t else Empty
  | Branch (p,m,t0,t1) -> branch (p, m, filter pr t0, filter pr t1)

let partition p s =
  let rec part (t,f as acc) = function
    | Empty -> acc
    | Leaf k -> if p k then (add k t, f) else (t, add k f)
    | Branch (_,_,t0,t1) -> part (part acc t0) t1
  in
  part (Empty, Empty) s

let rec choose = function
  | Empty -> raise Not_found
  | Leaf k -> k
  | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)

let elements s =
  let rec elements_aux acc = function
    | Empty -> acc
    | Leaf k -> k :: acc
    | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
  in
  (* unfortunately there is no easy way to get the elements in ascending
     order with little-endian Patricia trees *)
  List.sort Pervasives.compare (elements_aux [] s)

let split x s =
  let coll k (l, b, r) =
    if k < x then add k l, b, r
    else if k > x then l, b, add k r
    else l, true, r
  in
  fold coll s (Empty, false, Empty)

(*s There is no way to give an efficient implementation of [min_elt]
    and [max_elt], as with binary search trees.  The following
    implementation is a traversal of all elements, barely more
    efficient than [fold min t (choose t)] (resp. [fold max t (choose
    t)]). Note that we use the fact that there is no constructor
    [Empty] under [Branch] and therefore always a minimal
    (resp. maximal) element there. *)

let rec min_elt = function
  | Empty -> raise Not_found
  | Leaf k -> k
  | Branch (_,_,s,t) -> min (min_elt s) (min_elt t)

let rec max_elt = function
  | Empty -> raise Not_found
  | Leaf k -> k
  | Branch (_,_,s,t) -> max (max_elt s) (max_elt t)

(*s Another nice property of Patricia trees is to be independent of the
    order of insertion. As a consequence, two Patricia trees have the
    same elements if and only if they are structurally equal. *)

let equal v1 v2 = 0 == (compare v1 v2)

let compare = compare

(*i*)
let make l = List.fold_right add l empty
(*i*)

(*s Additional functions w.r.t to [Set.S]. *)

let rec intersect s1 s2 = match (s1,s2) with
  | Empty, _ -> false
  | _, Empty -> false
  | Leaf k1, _ -> mem k1 s2
  | _, Leaf k2 -> mem k2 s1
  | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
      if m1 == m2 && p1 == p2 then
        intersect l1 l2 || intersect r1 r2
      else if m1 < m2 && match_prefix p2 p1 m1 then
        intersect (if zero_bit p2 m1 then l1 else r1) s2
      else if m1 > m2 && match_prefix p1 p2 m2 then
        intersect s1 (if zero_bit p1 m2 then l2 else r2)
      else
        false


let to_string_aux elements s =
  let l = elements s in
  let rec aux acc = function
    | [] -> acc
    | x::q -> aux (","^(string_of_int x)^acc) q in
    match l with
      | [] -> "{}"
      | x::q -> Printf.sprintf "{%d%s}" x (aux "" q)

let to_string = to_string_aux elements
	  


(*s Big-endian Patricia trees *)

module Big = struct

  type elt = int

  type t_ = t
  type t = t_

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let singleton k = Leaf k

  let zero_bit k m = (k land m) == 0

  let rec mem k = function
    | Empty -> false
    | Leaf j -> k == j
    | Branch (_, m, l, r) -> mem k (if zero_bit k m then l else r)

  let mask k m  = (k lor (m-1)) land (lnot m)

  (* we first write a naive implementation of [highest_bit]
     only has to work for bytes *)
  let naive_highest_bit x =
    assert (x < 256);
    let rec loop i =
      if i = 0 then 1 else if x lsr i = 1 then 1 lsl i else loop (i-1)
    in
    loop 7

  (* then we build a table giving the highest bit for bytes *)
  let hbit = Array.init 256 naive_highest_bit

  (* to determine the highest bit of [x] we split it into bytes *)
  let highest_bit_32 x =
    let n = x lsr 24 in if n != 0 then hbit.(n) lsl 24
    else let n = x lsr 16 in if n != 0 then hbit.(n) lsl 16
    else let n = x lsr 8 in if n != 0 then hbit.(n) lsl 8
    else hbit.(x)

  let highest_bit_64 x =
    let n = x lsr 32 in if n != 0 then (highest_bit_32 n) lsl 32
    else highest_bit_32 x

  let highest_bit = match Sys.word_size with
    | 32 -> highest_bit_32
    | 64 -> highest_bit_64
    | _ -> assert false

  let branching_bit p0 p1 = highest_bit (p0 lxor p1)

  let join (p0,t0,p1,t1) =
    (*i let m = function Branch (_,m,_,_) -> m | _ -> 0 in i*)
    let m = branching_bit p0 p1 (*EXP (m t0) (m t1) *) in
    if zero_bit p0 m then
      Branch (mask p0 m, m, t0, t1)
    else
      Branch (mask p0 m, m, t1, t0)

  let match_prefix k p m = (mask k m) == p

  let add k t =
    let rec ins = function
      | Empty -> Leaf k
      | Leaf j as t ->
	  if j == k then t else join (k, Leaf k, j, t)
      | Branch (p,m,t0,t1) as t ->
	  if match_prefix k p m then
	    if zero_bit k m then
	      Branch (p, m, ins t0, t1)
	    else
	      Branch (p, m, t0, ins t1)
	  else
	    join (k, Leaf k, p, t)
    in
    ins t

  let remove k t =
    let rec rmv = function
      | Empty -> Empty
      | Leaf j as t -> if k == j then Empty else t
      | Branch (p,m,t0,t1) as t ->
	  if match_prefix k p m then
	    if zero_bit k m then
	      branch (p, m, rmv t0, t1)
	    else
	      branch (p, m, t0, rmv t1)
	  else
	    t
    in
    rmv t

  let rec merge = function
    | Empty, t  -> t
    | t, Empty  -> t
    | Leaf k, t -> add k t
    | t, Leaf k -> add k t
    | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
	if m == n && match_prefix q p m then
	  (* The trees have the same prefix. Merge the subtrees. *)
	  Branch (p, m, merge (s0,t0), merge (s1,t1))
	else if m > n && match_prefix q p m then
	  (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	  if zero_bit q m then
	    Branch (p, m, merge (s0,t), s1)
          else
	    Branch (p, m, s0, merge (s1,t))
	else if m < n && match_prefix p q n then
	  (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	  if zero_bit p n then
	    Branch (q, n, merge (s,t0), t1)
	  else
	    Branch (q, n, t0, merge (s,t1))
	else
	  (* The prefixes disagree. *)
	  join (p, s, q, t)

  let union s t = merge (s,t)

  let rec subset s1 s2 = match (s1,s2) with
    | Empty, _ -> true
    | _, Empty -> false
    | Leaf k1, _ -> mem k1 s2
    | Branch _, Leaf _ -> false
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
	  subset l1 l2 && subset r1 r2
	else if m1 < m2 && match_prefix p1 p2 m2 then
	  if zero_bit p1 m2 then
	    subset l1 l2 && subset r1 l2
	  else
	    subset l1 r2 && subset r1 r2
	else
	  false

  let rec inter s1 s2 = match (s1,s2) with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Leaf k1, _ -> if mem k1 s2 then s1 else Empty
    | _, Leaf k2 -> if mem k2 s1 then s2 else Empty
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
	  merge (inter l1 l2, inter r1 r2)
	else if m1 > m2 && match_prefix p2 p1 m1 then
	  inter (if zero_bit p2 m1 then l1 else r1) s2
	else if m1 < m2 && match_prefix p1 p2 m2 then
	  inter s1 (if zero_bit p1 m2 then l2 else r2)
	else
	  Empty

  let rec diff s1 s2 = match (s1,s2) with
    | Empty, _ -> Empty
    | _, Empty -> s1
    | Leaf k1, _ -> if mem k1 s2 then Empty else s1
    | _, Leaf k2 -> remove k2 s1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
	  merge (diff l1 l2, diff r1 r2)
	else if m1 > m2 && match_prefix p2 p1 m1 then
	  if zero_bit p2 m1 then
	    merge (diff l1 s2, r1)
	  else
	    merge (l1, diff r1 s2)
	else if m1 < m2 && match_prefix p1 p2 m2 then
	  if zero_bit p1 m2 then diff s1 l2 else diff s1 r2
	else
	  s1

  (* same implementation as for little-endian Patricia trees *)
  let cardinal = cardinal
  let iter = iter
  let fold = fold
  let for_all = for_all
  let exists = exists
  let filter = filter

  let partition p s =
    let rec part (t,f as acc) = function
      | Empty -> acc
      | Leaf k -> if p k then (add k t, f) else (t, add k f)
      | Branch (_,_,t0,t1) -> part (part acc t0) t1
    in
    part (Empty, Empty) s

  let choose = choose
  let choose_and_remove = choose_and_remove

  let elements s =
    let rec elements_aux acc = function
      | Empty -> acc
      | Leaf k -> k :: acc
      | Branch (_,_,l,r) -> elements_aux (elements_aux acc r) l
    in
    (* we still have to sort because of possible negative elements *)
    List.sort Pervasives.compare (elements_aux [] s)

  let split x s =
    let coll k (l, b, r) =
      if k < x then add k l, b, r
      else if k > x then l, b, add k r
      else l, true, r
    in
    fold coll s (Empty, false, Empty)

  (* could be slightly improved (when we now that a branch contains only
     positive or only negative integers) *)
  let min_elt = min_elt
  let max_elt = max_elt

  let equal = (=)

  let compare = compare

  let make l = List.fold_right add l empty

  let rec intersect s1 s2 = match (s1,s2) with
    | Empty, _ -> false
    | _, Empty -> false
    | Leaf k1, _ -> mem k1 s2
    | _, Leaf k2 -> mem k2 s1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	if m1 == m2 && p1 == p2 then
          intersect l1 l2 || intersect r1 r2
	else if m1 > m2 && match_prefix p2 p1 m1 then
          intersect (if zero_bit p2 m1 then l1 else r1) s2
	else if m1 < m2 && match_prefix p1 p2 m2 then
          intersect s1 (if zero_bit p1 m2 then l2 else r2)
	else
          false

  let to_string = to_string_aux elements

end

(*s Big-endian Patricia trees with non-negative elements only *)

module BigPos = struct

  include Big

  let singleton x = if x < 0 then invalid_arg "BigPos.singleton"; singleton x

  let add x s = if x < 0 then invalid_arg "BigPos.add"; add x s

  (* Patricia trees are now binary search trees! *)

  let rec mem k = function
    | Empty -> false
    | Leaf j -> k == j
    | Branch (p, _, l, r) -> if k <= p then mem k l else mem k r

  let rec min_elt = function
    | Empty -> raise Not_found
    | Leaf k -> k
    | Branch (_,_,s,_) -> min_elt s

  let rec max_elt = function
    | Empty -> raise Not_found
    | Leaf k -> k
    | Branch (_,_,_,t) -> max_elt t

  (* we do not have to sort anymore *)
  let elements s =
    let rec elements_aux acc = function
      | Empty -> acc
      | Leaf k -> k :: acc
      | Branch (_,_,l,r) -> elements_aux (elements_aux acc r) l
    in
    elements_aux [] s

  let to_string = to_string_aux elements

end

(*s EXPERIMENT: Big-endian Patricia trees with swapped bit sign *)

module Bigo = struct

  include Big

  (* swaps the sign bit *)
  let swap x = if x < 0 then x land max_int else x lor min_int

  let mem x s = mem (swap x) s

  let add x s = add (swap x) s

  let singleton x = singleton (swap x)

  let remove x s = remove (swap x) s

  let elements s = List.map swap (elements s)

  let choose s = swap (choose s)

  let iter f = iter (fun x -> f (swap x))

  let fold f = fold (fun x a -> f (swap x) a)

  let for_all f = for_all (fun x -> f (swap x))

  let exists f = exists (fun x -> f (swap x))

  let filter f = filter (fun x -> f (swap x))

  let partition f = partition (fun x -> f (swap x))

  let split x s = split (swap x) s

  let rec min_elt = function
    | Empty -> raise Not_found
    | Leaf k -> swap k
    | Branch (_,_,s,_) -> min_elt s

  let rec max_elt = function
    | Empty -> raise Not_found
    | Leaf k -> swap k
    | Branch (_,_,_,t) -> max_elt t

  let to_string = to_string_aux elements

end

let test empty add mem =
  let seed = Random.int max_int in
  Random.init seed;
  let s =
    let rec loop s i =
      if i = 1000 then s else loop (add (Random.int max_int) s) (succ i)
    in
    loop empty 0
  in
  Random.init seed;
  for i = 0 to 999 do assert (mem (Random.int max_int) s) done

