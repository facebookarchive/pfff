open Phylomel
open Printf

let () =
	if (Array.length Sys.argv) < 3 then
		printf "usage : geno2svg input_file output_file\n"
	else
		let geno_file = Sys.argv.(1) in
		let svg_file = Sys.argv.(2) in

		(* We create four things :
		     - genotypes collection
		     - distance matrix
		     - minimum spanning tree
		     - figure (graphical tree) *)

		let collec = Genotypes.read_file geno_file in
		let dmat = GenoMat.create collec in
		let tree = Tree.prim_complete collec dmat in
		let fig = Phylogram.radial_layout ~reframe:true 800. tree in

		Phylogram.write_svg_file collec fig svg_file

