open ExtLib

(**
   Barnes-Hut tree code
*)

(** Allows for n log(n) calculation on the n-body simulation *)

type body = {
	p : Vec2.t;
	v : Vec2.t;
}

(** Bounds are stored (for efficiency) as a structure.
	Each bound in the ith dimension is a half-open interval:
	  [x0] <= x < [x1]
      [y0] <= y < [y1] *)
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

(** [mass t] returns the mass of the tree [t] *)
val mass : tree -> float

(** [center t] returns the center of mass of the tree [t] *)
val center : tree -> Vec2.t

(** [in_bounds bds v] checks whether [v] is in the bounds given by
	[bds]. *)
val in_bounds : bounds -> body -> bool

val square : float -> float

(** [sub_bounds bds] returns an array of the 2^d (where d is the
	dimension of the space---[bs] is 2*d elements long) sub-bounds of
	the [bds] obtained by splitting the bounds in half on each
	dimension. *)
val sub_bounds : bounds -> bounds array

(** [make_null_bounds ()] returns a fresh set of bounds which enclose
	{b no} possible object. *)
val make_null_bounds : unit -> bounds

(** [bounds_of_bodies bs] returns a bounds which completely enclose
	the given bodies [bs]. *)
val bounds_of_bodies : body list -> bounds

(** [bounds_size_squared bds] returns the size squared of the given
	bounds (i.e. the sum of squares of distances along each
	dimension).*)
val bounds_size_squared : bounds -> float

(** [mass sts] @return the mass of the subtrees *)
val mass_trees : tree array -> float

(** [mass_barycenter sts]
	@return the mass and barycenter of the subtrees *)
val mass_barycenter : tree array -> float * Vec2.t

(** [tree_of_bodies bs]
	@return a tree containing the bodies [bs]. *)
val tree_of_bodies : body list -> tree

(** [tree_of_bodies_array bs]
@return a tree containing the bodies [bs]. *)
val tree_of_bodies_array : body array -> tree

(** [fold fn start t] is the fundamental tree iterator.  Alas, there
	is no guarantee what the order of application of [fn] is. *)
val fold : ('a -> tree -> 'a) -> 'a -> tree -> 'a

(** [contains b t] returns [true] if [t] contains [b]. *)
val contains : body -> tree -> bool

val do_add_force_on : Vec2.t -> float -> Vec2.t -> Vec2.t -> float -> unit

val do_calc_force : Vec2.t -> float ->body -> tree -> unit

val do_calc_forces : Vec2.t array -> float -> body array -> tree -> unit

