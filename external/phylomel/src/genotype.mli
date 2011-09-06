(** Genotype functions*)

(** This module implements a genotype and its common functions *)

type t = {
    id : string;
    markers : float array;
	infos : string array;
	  (** exemple : [|France; Paris; 2009|] *)
}

(** [size g] @return number of markers *)
val size : t -> int

(** [diff g1 g2] @return number of different markers *)
val diff : t -> t -> int

(** [markers_nb gs]
	@return
	 - [Some size] where size if the common number of markers
	   of the genotypes
	 - [None] if the genotypes have different sizes *)
val markers_nb : t array -> int option

(** [description g] @return the concatenation of [g.id] and [g.infos] *)
val description : t -> string

(** [equal_markers g1 g2] @return comparison of g1 and g2 markers
	(lexicographic order *)
val compare_markers : t -> t -> int

(** [equal_markers g1 g2] @return true if g1 and g2 have the same markers *)
val equal_markers : t -> t -> bool

(** Hash Table with genotypes as keys
	(equal genotypes = equal markers) *)
module GenoHash :
  sig
    type key = t
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
  end

module GenoSet :
sig
    type elt = t
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
end
