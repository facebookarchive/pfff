(**
   Diversity indices
*)


(** [simpson genos]
	@return a tuple :
	 - number of distinct genotypes
	 - simpson diversity index
*)
val simpson : Genotype.t array -> float
val simpson_confidence_interval : Genotype.t array -> float * (float * float)
