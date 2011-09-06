open Genotypes

type t = {
	adj_mat : bool DistMat.t;
	dist_mat : int DistMat.t;
	children : int list array;
	  (* children.(i) : children list of the ith vertex *)
	parents : int array;
}

let get_children m parent index =
	let l = ref [] in
	for i = (Array.length m) - 1 downto 0 do
		if (i <> parent) && (i <> index) then
			if DistMat.get m index i then
				l := i :: !l
	done;
	!l

let children_of_adj_mat m =
	let n = Array.length m in
	let children = Array.create n [] in
	let rec iterChildren parent index =
		let node_children = get_children m parent index in
		children.(index) <- node_children;
		List.iter (iterChildren index) node_children
	in iterChildren 0 0;
	children

let parents_of_children children =
	let n = Array.length children in
	let parents = Array.create n 0 in
	Array.iteri
		(fun i cs ->
			 List.iter (fun c -> parents.(c) <- i) cs)
		children;
	parents

let create adj_mat dist_mat =
	let children = children_of_adj_mat adj_mat in
	{ adj_mat = adj_mat;
	  dist_mat = dist_mat;
	  children = children;
	  parents = parents_of_children children }

let size t =
	Array.length t.parents

let leaves_nb t =
	let n = Array.length t.children in
	 (* [ls_nb.(i)] : number of leaves in the subtree [i] *)
	let ls_nb = Array.create n 0 in
	let rec iterChildren i =
		match t.children.(i) with
			| [] -> ls_nb.(i) <- 1
			| l ->
				  List.iter iterChildren l;
				  let leaves_nb =
					  List.fold_left
						  (fun acc i -> acc + ls_nb.(i))
						  0 l in
				  ls_nb.(i) <- leaves_nb in
	iterChildren 0;
	ls_nb

(* Minimum spanning tree with the prim algorithm
   on a fully connected tree (clique) *)
let prim_complete_adj_mat max (m: int array array) =
	let n = Array.length m in
	let tree = DistMat.create n false in
	let in_tree = Array.create n false in

	(* When the ith vertex is not in the tree :
	    - close.(i) is the closest node to i in the tree
	    - dist.(i)  is the distance between the two *)
	let close = Array.create n 0 in
	let dist = Array.create n (max+1) in

	(* After adding k to the tree, we update close and dist *)
	let update k =
		for i=0 to n-1 do
			if not in_tree.(i) && i <> k then (
				let dist' = DistMat.get m i k in
				if dist' < dist.(i) then (
					dist.(i) <- dist';
					close.(i) <- k
				)
			)
		done in

	in_tree.(0) <- true;
	for i=1 to n-1 do
		dist.(i) <- DistMat.get m 0 i
	done;

	for j=1 to n - 1 do

		(* Finds out [c], the closest node to the tree *) 

		let c = ref (-1) in
		for i=0 to n - 1 do
			if not in_tree.(i) then
				if !c = -1 || dist.(i) < dist.(!c) then
					c := i
		done;

		(* We add c to the tree *)
		in_tree.(!c) <- true;
		DistMat.set tree !c close.(!c) true;
		update !c

	done;

	tree

let prim_complete genocoll dist_mat =
	let adj_mat = prim_complete_adj_mat genocoll.geno_size dist_mat in
	create adj_mat dist_mat
