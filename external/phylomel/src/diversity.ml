module HashedGeno = struct
	type t = Genotype.t
	let equal g1 g2 = Genotype.diff g1 g2 = 0
	let hash g = Hashtbl.hash g.Genotype.tab
end

module H =
	Hashtbl.Make(HashedGeno)

(* Update the number of occurences of geno in [h] *)
let add_in_hash h geno =
	if H.mem h geno then (
		let occur = H.find h geno in
		H.replace h geno (occur + 1)
	) else
		H.add h geno 1

let occur_hash genos =
	let h = H.create 30 in
	Array.iter (fun g -> add_in_hash h g) genos;
	h

let simpson_with_hash genos h =
	(* sum_n is the sum of all n(n-1)
	   where n is the number of occurences of each genotype *)
	let fold geno n acc =
		acc + n * (n - 1) in
	let sum_n = H.fold fold h 0 in

	(* number of genotypes *)
	let n = Array.length genos in

	(* diversity index *)
	let foi = float_of_int in
	1. -. foi(sum_n) /. foi(n * (n - 1))

let simpson genos =
	simpson_with_hash genos (occur_hash genos)
	
let variance_with_hash genos h =
	let nb = Array.length genos in
	let f = float_of_int in
	let a1 = ref 0. in
	let a2 = ref 0. in
	let add_val g n =
		let pi = f(n) /. f(nb) in
		a1 := !a1 +. pi *. pi *. pi;
		a2 := !a2 +. pi *. pi in
	H.iter add_val h;
	4. /. f(nb) *. (!a1 -. !a2 *. !a2)
			 
let variance genos =
	variance_with_hash genos (occur_hash genos)

let simpson_confidence_interval_with_hash genos h =
	let d = simpson_with_hash genos h in
	let var = variance_with_hash genos h in
	d, (d -. 2. *. sqrt(var), d +. 2. *. sqrt(var))
	
let simpson_confidence_interval genos =
	simpson_confidence_interval_with_hash genos (occur_hash genos)
