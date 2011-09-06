open ExtArray

type t = {
    id : string;
    markers : float array;
	infos : string array;
}

let size g = Array.length g.markers

(* Returns number of different markers *)
let diff g1 g2 =
    let count = ref 0 in
	for i=0 to Array.length g1.markers - 1 do
		if g1.markers.(i) <> g2.markers.(i) then
			incr count;
	done;
	!count

(* Returns common number of markers *)
let markers_nb = function
	| [||] -> None
	| genos ->
	      let first_size = size genos.(0) in
	      let has_wrong_size g = (size g) <> first_size in
	      if Array.exists has_wrong_size genos then
			  None
		  else Some first_size

(* To print in a file *)
let description g =
	let b = Buffer.create 30 in
	let append s =
		Buffer.add_char b ' ';
		Buffer.add_string b s in
	append g.id;
	Array.iter append g.infos;
	Buffer.contents b

let compare_float_arrays (a1 : float array) (a2 : float array) =
	let n = Array.length a1 in
	let rec loop i =
		if i = n-1 then compare a1.(i) a2.(i)
		else
			let c = compare a1.(i) a2.(i) in
			if c <> 0 then c
			else loop (i+1)
	in loop 0

let compare_markers g1 g2 =
	compare_float_arrays g1.markers g2.markers

let equal_markers g1 g2 =
	compare_markers g1 g2 = 0

(* Avoids cycling type *)
type genotype = t

(* Intermediate modules *)

module HashedGenotype = struct
	type t = genotype
	let equal = equal_markers
	let hash g = Hashtbl.hash g.markers
end

module OrderedGenotype = struct
	type t = genotype
	let compare = compare_markers
end

module GenoHash = Hashtbl.Make(HashedGenotype)
module GenoSet = Set.Make(OrderedGenotype)
