open Phylomel
open Printf

let () =
    if (Array.length Sys.argv) < 3 then
		printf "usage : geno2svg input_file output_file\n"
    else
		(*
		  We extract 3 objects :
		    - the genotypes collection
		    - the distance matrix
		    - the dendogram
		*)
		let collec = Genotypes.read_file Sys.argv.(1) in
		let dmat = GenoMat.createF collec in
		let dendogram = Clustering.upgma dmat collec in
		ignore
			(Dendogram.write_svg_file Sys.argv.(2) dendogram)
