open ExtLib
open Vec2

type body = {
	p : Vec2.t;
	v : Vec2.t;
}

(** [max a b] is a local, float-specialized, version of the
	[Pervasives.max] function. *)
let max (a : float) (b : float) = 
	if a > b then a else b

(** [min a b] is a local, float-specialized, version of the
	[Pervasives.min] function. *)
let min (a : float) (b : float) = 
	if a < b then a else b

let square x = x *. x

let theta2 = 1.

type bounds =
    { mutable x0 : float;
	  mutable x1 : float;
	  mutable y0 : float;
	  mutable y1 : float; }

(** A cell contains, in order, the total mass, center of mass, cell
	bounds, cell size squared, sub-cells. *)
type cell =
	{ mass : float;
	  center : Vec2.t;
	  bds : bounds;
	  sq_size : float;
	  sub_ts : tree array }
and tree = 
	| Empty
	| Body of body
	| Cell of cell

let mass = function 
	| Empty -> 0.0
	| Body b -> 1.
	| Cell c -> c.mass

let center = function 
	| Empty -> Vec2.null ()
	| Body b -> b.p
	| Cell c -> c.center

(** [in_bounds bds b] checks whether the body [b] is in the bounds
	given by [bds]. *)
let in_bounds bds b =
	let p = b.p in
	   bds.x0 <= p.x && p.x < bds.x1
	&& bds.y0 <= p.y && p.y < bds.y1

(** [bit_set i n] is [true] if bit [i] of the integer [n] (bit 0 is
	the least-significant bit of [n]) is 1 and [false] otherwise. *)
let bit_set i n = 
	n land (1 lsl i) > 0 

(** [sub_bounds bds] returns an array of the 2^d (where d is the
	dimension of the space---[bs] is 2*d elements long) sub-bounds of
	the [bds] obtained by splitting the bounds in half on each
	dimension. *)
let sub_bounds bds = 
	Array.init 4
		(fun i ->
			 (* i now encodes (bitwise) whether the bound in each
				dimension is high or low *)
			 let sb = { x0=0.; x1=0.; y0=0.; y1=0. } in
			 (* x values *)
			 let mid = (bds.x0 +. bds.x1) /. 2. in
			 if bit_set 0 i then (
				 sb.x0 <- mid;
				 sb.x1 <- bds.x1;
			 ) else (
				 sb.x0 <- bds.x0;
				 sb.x1 <- mid;
			 );
			 (* y values *)
			 let mid = (bds.y0 +. bds.y1) /. 2. in
			 if bit_set 1 i then (
				 sb.y0 <- mid;
				 sb.y1 <- bds.y1;
			 ) else (
				 sb.y0 <- bds.y0;
				 sb.y1 <- mid;
			 );
			 sb)

(** [make_null_bounds ()] returns a fresh set of bounds which enclose
	{b no} possible object. *)
let make_null_bounds () =
	{ x0 = infinity;
	  x1 = neg_infinity;
	  y0 = infinity;
	  y1 = neg_infinity }
		
(** [expand q] creates a value which is a bit bigger than [q] so that
	[bounds_of_bodies bs] returns bounds which guarantee to enclose
	[bs]. *)
let expand q =
	let factor = sqrt epsilon_float in 
	q +. (abs_float q)*.factor

(** [bounds_of_bodies bs] returns a bounds which completely enclose
	the given bodies [bs]. *)
let bounds_of_bodies bs = 
	let bds = make_null_bounds () in 
	let update_bounds b =
		let p = b.p in
		bds.x0 <- min bds.x0 p.x;
		bds.x1 <- max bds.x1 (expand p.x);
		bds.y0 <- min bds.y0 p.y;
		bds.y1 <- max bds.y1 (expand p.y) in
	List.iter update_bounds bs;
	bds

(** [bounds_size_squared bds] returns the size squared of the given
	bounds (i.e. the sum of squares of distances along each
	dimension).*)
let bounds_size_squared bds = 
	let size = ref 0. in 
	size := !size +. (square bds.x1 -. bds.x0);
	size := !size +. (square bds.y1 -. bds.y0);
	!size
		
(** [mass_and_com sts] returns the mass and center-of-mass of the subtrees *)

(** [mass sts] @return the mass of the subtrees *)
let mass_trees ts = 
	let m = ref 0. in
	for i=0 to Array.length ts - 1 do
		m := !m +. (mass ts.(i))
	done;
	!m

(** [mass_barycenter sts] @return the mass and barycenter of the subtrees *)
let mass_barycenter ts =
	let x = ref 0. in
	let y = ref 0. in
	for i=0 to Array.length ts - 1 do
		let t = ts.(i) in
		x := !x +. (center t).x *. (mass t);
		y := !y +. (center t).y *. (mass t);
	done;
	let mass = mass_trees ts in
	x := !x /. mass;
	y := !y /. mass;
	mass, Vec2.make !x !y

(** [tree_of_bodies bs] @return a tree containing the bodies [bs]. *)
let rec tree_of_bodies = function
	| [] -> Empty
	| [b] -> Body(b)
	| bs -> 
		  let bds = bounds_of_bodies bs in
		  let s = bounds_size_squared bds in 
		  let sub_bds = sub_bounds bds in 
		  let sub_trees = Array.map
			  (fun bd ->
				   let sub_bs = List.filter (in_bounds bd) bs in
				   tree_of_bodies sub_bs)
			  sub_bds in
		  let m, center = mass_barycenter sub_trees in
		  Cell { mass = m;
				 center = center;
				 bds = bds;
				 sq_size = s;
				 sub_ts = sub_trees }
			  
(** [tree_of_bodies bs] returns the tree which contains [bs]. *)
let tree_of_bodies_array bs = 
	let lbs = Array.to_list bs in 
	tree_of_bodies lbs

(** [fold fn start t] is the fundamental tree iterator.  Alas, there
	is no guarantee what the order of application of [fn] is. *)
let rec fold fn start t =
	match t with 
		| Empty -> start
		| Body _ -> fn start t
		| Cell c ->
			  Array.fold_left
				  (fold fn) (fn start t) c.sub_ts

(** [fold_w_abort fn start t] folds [fn] over [t] (with initial value
	[start]).  [fn] is applied to each tree-node before it is applied
	to the sub-nodes.  If [fn] returns [(false, value)] value is
	returned as the result of the fold for this entire branch---the
	recursion stops *)
let rec fold_w_abort fn start t = 
	match t with 
		| Empty -> start
		| Body _ -> snd (fn start t)
		| Cell c ->
			  let (cont, new_start) = fn start t in 
			  if cont then 
				  Array.fold_left (fold_w_abort fn) new_start c.sub_ts
			  else
				  new_start

(** [contains b t] returns [true] if [t] contains [b]. *)
let rec contains b t = 
	match t with 
		| Empty -> false
		| Body b' -> b == b'
		| Cell c ->
			  if not (in_bounds c.bds b) then false
			  else
				  Array.exists (contains b) c.sub_ts

(*
  let bs = [make 1. 1.; make 10. 4.; make 3. 5.];;
  let t = tree_of_bodies bs in
  List.for_all (fun b -> contains b t) bs;;
*)

let do_add_force_on f k p p' m' =
	let m = 1. in
	let dx = p'.x -. p.x in
	let dy = p'.y -. p.y in
	let r = sqrt (square dx +. square dy) in
	let r3 = r *. r *. r in
	let factor = k *. m *. m' in
	f.x <- f.x -. (factor *. dx /. (r3 +. 0.3));
	f.y <- f.y -. (factor *. dy /. (r3 +. 0.3))

let dist2 v1 v2 =
	sqrt (square (v2.x -. v1.x) +. square (v2.y -. v1.y))

let do_calc_force f k b tree =
	let rec calc_force' = function
		| Body b' ->
			  if b <> b' then
				  do_add_force_on f k b.p b'.p 1.
		| Cell c ->
			  let r2 = dist2 b.p c.center in
			  if c.sq_size /. r2 < theta2 then
				  do_add_force_on f k b.p c.center c.mass
			  else
				  Array.iter calc_force' c.sub_ts
		| Empty -> () in
	calc_force' tree
			  
let do_calc_forces fs k bs tree =
	for i=0 to Array.length fs - 1 do
		do_calc_force fs.(i) k bs.(i) tree
	done
