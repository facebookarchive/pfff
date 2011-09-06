open Printf
open ExtLib

let print_line line =
	DynArray.iter (printf "%3.2f ") line;
	printf "\n"
		
let print mat =
	DynArray.iter print_line mat
			
let get mat i j =
	DynArray.get (DynArray.get mat i) j

let unsafe_get mat i j =
	DynArray.unsafe_get (DynArray.unsafe_get mat i) j
		
let rem_col mat j =
	for i = j+1 to (DynArray.length mat) - 1 do
		DynArray.delete (DynArray.get mat i) j
	done

let of_mat mat =
	let dyn_mat = DynArray.create () in
	Array.iter (fun x -> DynArray.add dyn_mat (DynArray.of_array x)) mat;
	dyn_mat

(*Retrieves the difference beetween nodes of index i and j in a dynamic matrix*)
let get_diff m i j =
	if i = j then 0.0 else
		if i > j then get m i j
		else		  get m j i
