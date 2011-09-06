open ExtLib

(** Handles phylogenetic minimum spanning trees *)

type t = {
	adj_mat : bool DistMat.t;  (** Adjacency matrix *)
	dist_mat : int DistMat.t;  (** Distance matrix  *)
	children : int list array; (** Children list    *)
	parents : int array;       (** Parents list     *)
}

(** [create adj_mat dist_mat] @return a new tree *)
val create : bool DistMat.t -> int DistMat.t -> t

(** [size t] @return the number of egdes in the tree *)
val size : t -> int

(** [leaves_nb t] @return an array where the ith element is the number
    of leaves in the subtree where the ith vertex is the root *)
val leaves_nb : t -> int array

(** Prim's algorithm finds a minimum spanning tree on a genotypes collection,
	given a distance matrix *)
val prim_complete : Genotypes.t -> int DistMat.t -> t
