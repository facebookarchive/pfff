(**
 Types and common functions for dendograms *)

(**
   A dendogram is an unrooted tree for visual classification of similarity.
   Here we define its type and common basic functions.

   The homology is the percentage of similarity.
*)

(** {4 Types} *)

(** The tree itself is defined as an algebraic datatype *)

type leaf = {
	geno : int;          (** Each leaf maps the genotype of index value *)
	mutable index : int; (** Position of the genotype in the dendogram  *)
}

type node = {
	t1 : tree;
	t2 : tree;
	height : int;
	leaves_nb : int;     (** Number of leaves in the subtree *)
	homology : float;    (** Homology between the t1 and t2  *)
	mutable pos : float; 
	  (** Position of the node along the y-axis in the dendogram *)
}

and tree =
	| Leaf of leaf
	| Node of node

(** A dendogram encapsulates a tree with values needed to print it *)

type t = {
	coll : Genotypes.t;
	tree : tree;
	leaves : leaf list;
}

val create : Genotypes.t -> tree -> leaf list -> t

(** {5 Constructors} *)

(** [mkLeaf i]
	@return a new leaf pointing to the genotype of index i
*)
val mk_leaf : int -> tree

(** [mkNode t1 t2 h]
	@return a new node of subtrees [t1] and [t2],
	h being the homology between [t1] and [t2]
*)
val mk_node : tree -> tree -> float -> node

(** {4 Accessors} *)

(** [height tree]
	@return the height of a tree (0 for leaves) *)
val height : tree -> int

(** [leavesNb tree]
	@return the number of leaves in a tree *)
val leaves_nb : tree -> int

(** [getPos tree]
	@return the vertical position of a node *)
val get_pos : tree -> float

(** [getHomology tree]
	@return the homology between the subtrees of a node *)
val get_homology : tree -> float

(**[homology geno_size diff]
   @return
   the homology between two genotypes of length [geno_size] with [diff]
   differences  *)
val homology : float -> int -> float

(** [minHomology tree]
	@return the minimum homology in the tree
	(allowing us to tune the graphical output) *)
val min_homology : tree -> float

(** {4 Printing} *)

(** A transform function allows us to transform 2D coordinates *)
type transform = float * float -> float * float

(** 
	The [link_info] type allows us to specify links on the genotypes of the
	graphical output.

	 - [string array] : hypertext links
	 - [string] : target field in each hypertext link *)
type links = string array * string

(**
   [header width height]
   @return an Svg header
*)
val header : int -> int -> string

(** Below are functions writing svg.
	They use ExtLib's higher-order abstract io *)

(** Variables named ddg are dendograms *)

(**
   [write_svg out ~links_info ddg]
   prints the tree as an svg picture in [out]
   @return output, width, height
*)
val write_svg :
    'a IO.output ->
    ?links_info:links option ->
	t ->
	'a * int * int

(**
   [write_svg_file file ~links_info ddg]
   prints the tree as an svg picture in [file] and closes it
   @return width, height
*)
val write_svg_file :
	string ->
	?links_info:links option ->
	t ->
	int * int

(** {3 Other functions of internal use} *)

val write_svg_leaves :
  'a IO.output ->
  ?links_info:links option ->
  t ->
  transform ->
  unit

 val write_svg_nodes :
  'a IO.output -> 
  t ->
  transform ->
  unit
