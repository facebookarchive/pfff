(** Genetic distance matrices *)

(** Functions postfixed with an "F" deal with float matrices *)

(** {4 Integer distance matrices} *)

(** [create collec]
    @return a distance matrix inferred from a collection of genotypes *)
val create : Genotypes.t -> int array array

(** [get matrix i j]
	@return genetic distance between the genotypes of index [i] and [j] *)
val get : int array array -> int -> int -> int

(** [empty dim]
	@return a distance matrix of size [dim] filled with 0s *)
val empty : int -> int array array

(** {4 Float distance matrices} *)

(** [createF collec]
	* @return a float distance matrix inferred
	from a collection of genotypes *)
val createF : Genotypes.t -> float array array

(** [emptyF dim]
	@return a distance matrix of size [dim] filled with 0.0 *)
val emptyF : int -> float array array

(** [getF matrix i j]
	@return floating genetic distance between the genotypes of index [i] and [j] *)
val getF : float array array -> int -> int -> float

(** [toFloat m] maps [float_of_int] on [m] *)
val toFloat : int array array -> float array array

(** [createP (genos, gsize)]
	@param genos genotype array
	@param gsize size of the genotypes in [genos]
	@return a distance matrix filled with difference percentages *)
val createP : Genotypes.t -> float array array

(** {4 Other functions} *)

val print : int array array -> unit
val printF : float array array -> unit
