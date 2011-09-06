open Phylomel
open Printf

let () =
	if Array.length Sys.argv < 2 then
		printf "usage : simpson genofile\n"
	else 
		let genos, _ = Genotype.read_file Sys.argv.(1) in
		let index, (i1, i2) =
			Diversity.simpson_confidence_interval genos in
		printf "Simpson diversity index : %f\n" index;
		printf "Confidence Interval : %f, %f\n" i1 i2
