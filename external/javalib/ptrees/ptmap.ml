(*
 * Copyright (C) 2008 Jean-Christophe Filliatre
 * Copyright (C) 2008, 2009 Laurent Hubert (CNRS)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)


(*s Maps of integers implemented as Patricia trees, following Chris
  Okasaki and Andrew Gill's paper {\em Fast Mergeable Integer Maps}
  ({\tt\small http://www.cs.columbia.edu/\~{}cdo/papers.html\#ml98maps}).
  See the documentation of module [Ptset] which is also based on the
  same data-structure. *)


module type S = sig
  type (+'a) t
  type key = int
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : ?merge:('a -> 'a -> 'a) -> int -> 'a -> 'a t -> 'a t
  val modify : int -> ('a option -> 'a) -> 'a t -> 'a t
  val find : int -> 'a t -> 'a
  val findi_element: (int -> 'a -> bool) -> 'a t -> int * 'a
  val find_element: ('a -> bool) -> 'a t -> 'a
  val remove : int -> 'a t -> 'a t
  val mem :  int -> 'a t -> bool
  val iter : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val merge_first : 'a t -> 'a t -> 'a t
  val diff : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  val choose_and_remove : 'a t -> int * 'a * ('a t)
  val inter : 'a t -> 'a t -> 'a t
  val inter_map2 : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val subset : 'a t -> 'a t -> bool
  val cardinal: 'a t -> int
  val exists: (int -> 'a -> bool) -> 'a t -> bool
  val filter: ('a -> bool) -> 'a t -> 'a t
  val filteri: (int -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
  val elements: 'a t -> (int * 'a) list
end

type key = int

type 'a t =
  | Empty
  | Leaf of int * 'a
  | Branch of int * int * 'a t * 'a t

let empty = Empty

let is_empty t = t = Empty

let zero_bit k m = (k land m) == 0

let rec mem k = function
  | Empty -> false
  | Leaf (j,_) -> k == j
  | Branch (_, m, l, r) -> mem k (if zero_bit k m then l else r)

let rec find k = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if k == j then x else raise Not_found
  | Branch (_, m, l, r) -> find k (if zero_bit k m then l else r)

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
    if zero_bit p0 m then
      Branch (mask p0 m, m, t0, t1)
    else
      Branch (mask p0 m, m, t1, t0)

let match_prefix k p m = (mask k m) == p

let modify k f t =
  let rec ins = function
    | Empty -> Leaf (k,f None)
    | Leaf (j,x') as t ->
	if j == k then
	  Leaf (k,f (Some x'))
	else
	  join (k, Leaf (k,f None), j, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k, Leaf (k,f None), p, t)
  in
    ins t

let add ?(merge=fun _a b -> b) k x t =
  let rec ins = function
    | Empty -> Leaf (k,x)
    | Leaf (j,x') as t ->
	if j == k then
	  Leaf (k,merge x' x)
	else
	  join (k, Leaf (k,x), j, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k, Leaf (k,x), p, t)
  in
    ins t

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let rec rmv = function
    | Empty -> Empty
    | Leaf (j,_) as t->
	if k == j then Empty
	else t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k p m then
	  if zero_bit k m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in rmv t

let rec choose_and_remove = function
  | Empty -> raise Not_found
  | Leaf (j,d) -> (j,d,Empty)
  | Branch (p,m,t0,t1) ->
      let (j,d,t0') = choose_and_remove t0
      in (j,d,branch (p,m,t0',t1))

let rec iter f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec map f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f x)
  | Branch (p,m,t0,t1) -> Branch (p, m, map f t0, map f t1)

let rec mapi f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f k x)
  | Branch (p,m,t0,t1) -> Branch (p, m, mapi f t0, mapi f t1)

let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf (k,x) -> f k x accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

(* we order constructors as Empty < Leaf < Branch *)
let compare cmp t1 t2 =
  let rec compare_aux t1 t2 = match t1,t2 with
    | t1,t2 when t1 == t2 -> 0
    | Empty, Empty -> 0
    | Empty, _ -> -1
    | _, Empty -> 1
    | Leaf (k1,x1), Leaf (k2,x2) ->
	let c = compare k1 k2 in
	  if c <> 0 then c else cmp x1 x2
    | Leaf _, Branch _ -> -1
    | Branch _, Leaf _ -> 1
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	let c = compare p1 p2 in
	  if c <> 0 then c else
	    let c = compare m1 m2 in
	      if c <> 0 then c else
		let c = compare_aux l1 l2 in
		  if c <> 0 then c else
		    compare_aux r1 r2
  in
    compare_aux t1 t2

let equal eq t1 t2 =
  let rec equal_aux t1 t2 = match t1, t2 with
    | Empty, Empty -> true
    | Leaf (k1,x1), Leaf (k2,x2) -> k1 = k2 && eq x1 x2
    | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	(p1 = p2 &&
	    m1 = m2 &&
	    (equal_aux l1 l2) &&
	    (equal_aux r1 r2))
    | _ -> false
  in
    equal_aux t1 t2

(* mostly taken from ptset.merge *)
let merge data_join t1 t2 =
  let rec merge t1 t2 =
    if t1==t2 then t1
    else
      match t1, t2 with
	| Empty, t
	| t, Empty -> t
	| Leaf (k,x), t
	| t, Leaf (k,x) -> add ~merge:data_join k x t
	| (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
	    if m == n && match_prefix q p m then
	      (* The trees have the same prefix. Merge the subtrees. *)
	      Branch (p, m, merge s0 t0, merge s1 t1)
	    else if m < n && match_prefix q p m then
	      (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	      if zero_bit q m then
		Branch (p, m, merge s0 t, s1)
              else
		Branch (p, m, s0, merge s1 t)
	    else if m > n && match_prefix p q n then
	      (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	      if zero_bit p n then
		Branch (q, n, merge s t0, t1)
	      else
		Branch (q, n, t0, merge s t1)
	    else
	      (* The prefixes disagree. *)
	      join (p, s, q, t)
  in
    merge t1 t2

(* [{(k,d)|((k,d) \in t1 \/ (k,d) \in t2) /\ (\exists d',(k,d') \in t1 => d=d')}] *)
let merge_first t1 t2 =
  let rec merge_first t1 t2 =
    if t1==t2 then false,t1
    else
      match t1, t2 with
	| _, Empty -> false, t1
	| Empty, _ -> true, t2
        | Leaf (k,_), Leaf (k',_) ->
            if k==k' then false,t1 else true,join (k, t1,k',t2)
	| Leaf (k,x), t ->
            true,add k x t
	| t, Leaf (k,x) ->
            let modifies = ref true in
            let t' = add ~merge:(fun a _ -> modifies:=false; a) k x t
            in
              !modifies, if !modifies then t' else t
	| (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
	    if m == n && match_prefix q p m then
	      (* The trees have the same prefix. Merge the subtrees. *)
              let (ml,l) = merge_first s0 t0
              and (mr,r) = merge_first s1 t1
              in
                if ml || mr
                then true,Branch (p, m, (if ml then l else s0), if mr then r else s1)
                else false,s
	    else if m < n && match_prefix q p m then
	      (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	      if zero_bit q m then
                let (ml,l) = merge_first s0 t
                in if ml then true,Branch (p, m, l, s1) else false,s
              else
                let (mr,r) = merge_first s1 t
                in if mr then true,Branch (p, m, s0, r) else false,s
	    else if m > n && match_prefix p q n then
	      (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	      if zero_bit p n then
                let (ml,l) = merge_first t0 s
                in if ml then true,Branch (q, n, l, t1) else true,t
	      else
                let mr,r = merge_first t1 s
                in if mr then true,Branch (q, n, t0, r) else true,t
	    else
	      (* The prefixes disagree. *)
	      true,join (p, s, q, t)
  in
    snd (merge_first t1 t2)

let rec inter s1 s2 =
  if s1 == s2 then s1 else
    match s1, s2 with
      | Empty , _ -> Empty
      | _, Empty -> Empty
      | Leaf (k1,_), Leaf (k2,_) ->
          if k1 == k2 then s1 else Empty
      | Leaf (k,_) as l, t -> if mem k t then l else Empty
      | t, Leaf (k,_) ->
          (try Leaf (k,(find k t))
           with Not_found -> Empty)
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
          if m1 == m2 && p1 == p2 then
            merge_first (inter l1 l2) (inter r1 r2)
          else if m1 < m2 && match_prefix p2 p1 m1 then
            inter (if zero_bit p2 m1 then l1 else r1) s2
          else if m1 > m2 && match_prefix p1 p2 m2 then
            inter s1 (if zero_bit p1 m2 then l2 else r2)
          else
            Empty

let rec inter_map2 f s1 s2 =
  if s1 == s2 then s1 else
    match s1, s2 with
      | Empty , _ -> Empty
      | _, Empty -> Empty
      | Leaf (k1,v1), Leaf (k2,v2) ->
          if k1 == k2 then Leaf (k2,f v1 v2) else Empty
      | Leaf (k,v), t -> 
          (try Leaf (k,f v (find k t))
           with Not_found -> Empty)
      | t, Leaf (k,v) ->
          (try Leaf (k,f (find k t) v)
           with Not_found -> Empty)
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
          if m1 == m2 && p1 == p2 then
            merge_first (inter_map2 f l1 l2) (inter_map2 f r1 r2)
          else if m1 < m2 && match_prefix p2 p1 m1 then
            inter_map2 f (if zero_bit p2 m1 then l1 else r1) s2
          else if m1 > m2 && match_prefix p1 p2 m2 then
            inter_map2 f s1 (if zero_bit p1 m2 then l2 else r2)
          else
            Empty

let rec subset s1 s2 =
  if s1 == s2 then true else
    match s1, s2 with
      | Empty , _ -> true
      | _, Empty -> false
      | Leaf (k1,_), Leaf (k2,_) ->
          if k1 == k2 then true else false
      | Leaf (k,_), t -> if mem k t then true else false
      | _ , Leaf (_,_) -> false
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
          if m1 == m2 && p1 == p2 then
            (subset l1 l2) && (subset r1 r2)
          else if m1 < m2 && match_prefix p2 p1 m1 then
            false
          else if m1 > m2 && match_prefix p1 p2 m2 then
            subset s1 (if zero_bit p1 m2 then l2 else r2)
          else
            false

let diff f t1 t2 =
  let remove_eq k a f t =
    let rec rmv = function
      | Empty -> Empty
      | Leaf (j,b) as t->
	  if (k == j) && (f a b) then Empty
	  else t
      | Branch (p,m,t0,t1) as t ->
	  if match_prefix k p m then
	    if zero_bit k m then
	      branch (p, m, rmv t0, t1)
	    else
	      branch (p, m, t0, rmv t1)
	  else
	    t
    in rmv t
  in
  let id = fun x _ -> x in
  let rec diff t1 t2 =
    match (t1,t2) with
      | Empty, _ -> Empty
      | _, Empty -> t1
      | Leaf (k1,a), _ ->
	  (try
	     let b = find k1 t2 in
	       if (f a b) then Empty
	       else t1
	   with _ -> t1)
      | _, Leaf (k2,a) -> remove_eq k2 a f t1
      | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
	  if m1 == m2 && p1 == p2 then
	    merge id (diff l1 l2) (diff r1 r2)
	  else if m1 < m2 && match_prefix p2 p1 m1 then
	    if zero_bit p2 m1 then
	      merge id (diff l1 t2) r1
	    else
	      merge id l1 (diff r1 t2)
	  else if m1 > m2 && match_prefix p1 p2 m2 then
	    if zero_bit p1 m2 then diff t1 l2 else diff t1 r2
	  else
	    t1
  in diff t1 t2

let rec exists p = function
  | Empty -> false
  | Leaf (k,a) -> p k a
  | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

let rec findi_element p = function
  | Empty -> raise Not_found
  | Leaf (k,a) -> if p k a then (k,a) else raise Not_found
  | Branch (_,_,t0,t1) -> 
      try 
	findi_element p t0 
      with Not_found -> 
	findi_element p t1

let rec find_element p = function
  | Empty -> raise Not_found
  | Leaf (_,a) -> if p a then a else raise Not_found
  | Branch (_,_,t0,t1) -> 
      try 
	find_element p t0 
      with Not_found -> 
	find_element p t1

(* old implementation *)
(* let rec filter pr = function *)
(*   | Empty -> Empty *)
(*   | Leaf (_,a) as t -> if pr a then t else Empty *)
(*   | Branch (p,m,t0,t1) -> branch (p, m, filter pr t0, filter pr t1) *)

(* implementation which keeps as much of the original tree (same addresses) *)
let filter pr map =
  let rec filter = function
    | Empty -> false,Empty
    | Leaf (_,a) as t -> if pr a then (false,t) else (true,Empty)
    | Branch (p,m,t0,t1) as b ->
        let (m0,t0') = filter t0
        and (m1,t1') = filter t1
        in
          if m0 || m1
          then
            let t0 = if m0 then t0' else t0
            and t1 = if m1 then t1' else t1
            in true, branch (p,m, t0, t1)
          else
            false, b
  in snd (filter map)

let filteri pr map =
  let rec filteri = function
    | Empty -> false,Empty
    | Leaf (k,a) as t -> if pr k a then (false,t) else (true,Empty)
    | Branch (p,m,t0,t1) as b ->
        let (m0,t0') = filteri t0
        and (m1,t1') = filteri t1
        in
          if m0 || m1
          then
            let t0 = if m0 then t0' else t0
            and t1 = if m1 then t1' else t1
            in true, branch (p,m, t0, t1)
          else
            false, b
  in snd (filteri map)

let rec filter_map pr = function
  | Empty -> Empty
  | Leaf (k,a) ->
      (match pr a with
	 | None -> Empty
	 | Some e -> Leaf (k,e)
      )
  | Branch (p,m,t0,t1) -> branch (p, m, filter_map pr t0, filter_map pr t1)

let partition p s =
  let rec part (t,f as acc) = function
    | Empty -> acc
    | Leaf (k,a) -> if p a then (add k a t, f) else (t, add k a f)
    | Branch (_,_,t0,t1) -> part (part acc t0) t1
  in
    part (Empty, Empty) s

let rec cardinal = function
  | Empty -> 0
  | Leaf (_,_) -> 1
  | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

let elements s =
  let rec elements_aux acc = function
    | Empty -> acc
    | Leaf (k,a) -> (k,a) :: acc
    | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
  in
    elements_aux [] s
