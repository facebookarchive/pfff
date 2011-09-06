(** Collection of genotypes *)

(** Please read first the Genotype module documentation*)

(** This module implements operations on collection of genotypes *)

(** Convention : a variable named "c" is a collection of genotypes *)

(** {4 Main type and functions} *)

(** Genotype collection *)
type t = {
	genos : Genotype.t array;
	fields : string array; (**exemple : [|"Country";"City";"Year"|]*)
	geno_size : int; (** genos' number of markers *)
	size : int; (** number of genotypes *)
}

(** [check_fields_nb c] @return true if the genotypes all have the
	right number of infos (which is the size of [c.fields]) *)
val check_fields_nb : t -> bool

(** [create genotypes fields ]
	@return a new genotypes collection,
	checks that all genotypes have the same size
	and the right fields number
	@raises Failure "Genotypes.create: failure reason" *)
val create : Genotype.t array -> string array -> t

val remove_duplicates : t -> t

(** {4 Reading files} *)

(** Exemple of markers' string representation : 1,4,3,5.6,8 *)

(** Format of a line in a file: genotype_id;markers;info1;info2;... *)
(** exemple : 12;2,4,4,5;France;Paris;2009 *)
(** Each line has at least a genotype id and markers *)

(** [read_markers s]
	@return an array of markers read from s
	@raise Invalid_argument "Genotypes.read_markers" *)
val read_markers : string -> float array

(** [read_line s]
	@return a genotype
	@raise Invalid_argument "Genotypes.read_line" *)
val read_line : string -> Genotype.t

(** [read_lines lines] reads an enumeration of lines
	@return the collection
	@raise Invalid_argument "Genotypes.read_lines"*)
val read_lines : string Enum.t -> t

(** [read_file file_name] reads a file
	@return the collection *)
val read_file : string -> t

(* - a line has the wrong format
   - there are different markers sizes between genotypes
   - a genotype does not have the right number of fields *)
