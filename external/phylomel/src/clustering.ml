open Printf
open ExtLib
open Genotypes
open Dendogram

(**
   Creation of UPGMA trees
*)

type tree = Dendogram.tree
type leaves = Dendogram.leaf list

(* The leaves who come first in the list are the ones belonging to the
   subtrees with the greatest number of leaves. *)
let rec leaves_of_tree_acc acc = function
	| Leaf(l) -> l::acc
	| Node(n) ->
		  if leaves_nb n.t1 >= leaves_nb n.t2 then
			  let acc' = leaves_of_tree_acc acc n.t1 in
			  leaves_of_tree_acc acc' n.t2
		  else
			  let acc' = leaves_of_tree_acc acc n.t2 in
			  leaves_of_tree_acc acc' n.t1

let leaves_of_tree t =
	List.rev (leaves_of_tree_acc [] t)

(* UNPURE
   [fillTreePos tree]
   Fills the vertical position of each node in the tree.
   The position of the leaves are filled first.
   The position of a node is the mean of the positions
   of its two subtrees.
*)
let rec fill_tree_pos tree =
	let leaves = leaves_of_tree tree in

	(*Fills the positions of the leaves*)
	List.iteri (fun i leaf -> leaf.index <- i) leaves;

	(*Fills the position of the nodes*)
	let rec fill_node node = match node with
		| Leaf(l) ->
			float_of_int l.index
		| Node(n) ->
			let pos1 = fill_node n.t1 in
			let pos2 = fill_node n.t2 in
			n.pos <- (min pos1 pos2) +. (abs_float (pos2 -. pos1)) /. 2.;
			n.pos
	
	in ignore(fill_node tree);
	   leaves

let find_min_coords dynmat =
	let min_i = ref 0 in
	let min_j = ref 0 in
	let min_diff = ref infinity in
	let size = (DynArray.length dynmat) - 1 in
	for i=0 to size - 1 do
		for j=0 to i-1 do
			let found_diff = DynMat.get dynmat i j in
			if (found_diff <= !min_diff) then (
				min_diff := found_diff;
				min_i := i;
				min_j := j
			)
		done
	done;
	(!min_i, !min_j), !min_diff

(*
  Builds an upgma tree from a difference matrix
  see the wikipedia article on upgma
*)
let upgma dmat collec =
	let matrix = DynMat.of_mat dmat in
	let nodes = DynArray.init collec.size mk_leaf in
	
	while (DynArray.length nodes) > 1 do
		let min_a = ref 0 in
		let min_b = ref 0 in
		let min_diff = ref infinity in
		let size = DynArray.length matrix in
		
		for i=0 to size-1 do
			for j=0 to (i-1) do
				let found_diff = DynMat.get matrix i j in
				if (found_diff <= !min_diff) then (
					min_diff := found_diff;
					min_a := i;
					min_b := j
				)
			done
		done;
		(*
		  Doest not work, don't know why.
		  let (min_a, min_b), min_diff = find_min_coords matrix in
		  let size = DynArray.length matrix in
		*)
		
		(*0. Creates the new node from the 2 closest nodes*)
		let node_a = DynArray.get nodes !min_a in
		let node_b = DynArray.get nodes !min_b in
		let homology = homology !min_diff collec.geno_size in
		let new_node = mk_node node_a node_b homology in
		
		(*1. Generates the new line
		     (with the differences to the new node)*)
		let new_line = DynArray.create () in
		for j=0 to size-1 do
			if (j <> !min_a) && (j <> !min_b) then (
				let leaves_a = float_of_int (leaves_nb node_a) in
				let leaves_b = float_of_int (leaves_nb node_b) in
				let sum = leaves_a +. leaves_b in
				let diff_a = DynMat.get_diff matrix j !min_a in
				let diff_b = DynMat.get_diff matrix j !min_b in
				let new_diff =
					( diff_a *. leaves_a +. diff_b *. leaves_b ) /. sum in
				
				DynArray.add new_line new_diff
			)
		done;
		
		(*2. Removes from the matrix the lines and columns
		     corresponding to node_a and node_b*)
		DynMat.rem_col matrix !min_a;
		DynMat.rem_col matrix !min_b;
		DynArray.delete matrix !min_a;
		DynArray.delete matrix !min_b;
		
		(*3. Adds the new line*)
		DynArray.add matrix new_line;
		
		(*4. Removes the nodes node_a and node_b from the node dynarray*)
		DynArray.delete nodes !min_a;
		DynArray.delete nodes !min_b;
		
		(*5. Adds the new node to the node dynarray*)
		DynArray.add nodes (Node(new_node))
	done;

	(* Now the only element in the node dynarray is the tree *)
	let tree = DynArray.get nodes 0 in
	(* Unpure, fills vertical positions of the nodes*)
	let leaves = fill_tree_pos tree in
	Dendogram.create collec tree leaves

