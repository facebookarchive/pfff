open Printf
open ExtLib

(** Creates phylogenetic trees with clustering methods (UPGMA, WPGMA) *)

(**
   We use the data types provided by the [Unrooted] module.
*)

(** {4 Building trees } *)

type tree = Dendogram.tree
type leaves = Dendogram.leaf list

(** [leavesOfTree tree]
	@return the leaves of the tree in a list,
	sorted to make sure that leaves belonging to the same node are
	contiguous in the tree. *)
val leaves_of_tree : tree -> leaves

(** [upgma distance_matrix genotypes_collection]
	@return an unrooted tree built with the UPGMA method *)
val upgma : float array array -> Genotypes.t -> Dendogram.t
