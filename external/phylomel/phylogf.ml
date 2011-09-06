open Phylomel
open Phylogram
open Vec2
open BarnesHut
open Phylogram

open Printf

let update_state n fs bs fig =
	let delta = 0.05 in

	(* Update forces *)
	ForceDirectedLayout.do_calc_forces fs bs fig;
	
	(* Euler integration on each body *)
	for i=0 to n - 1 do
		let b = bs.(i) in
		let f = fs.(i) in
		b.p.x <-
			b.p.x +. delta *. b.v.x +. 1./.2. *. delta *. delta *. f.x;
		b.p.y <-
			b.p.y +. delta *. b.v.y +. 1./.2. *. delta *. delta *. f.y;
		b.v.x <- b.v.x +. delta *. f.x;
		b.v.y <- b.v.y +. delta *. f.y;
		f.x <- 0.;
		f.y <- 0.
	done

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
		let collec = Genotypes.remove_duplicates collec in
		let dmat = GenoMat.create collec in
		let tree = Tree.prim_complete collec dmat in
		let fig = Phylogram.radial_layout ~reframe:false 800. tree in

		(* Creates force array, bodies *)
		let n = Phylogram.size fig in
		let fs = Array.init n (fun _ -> Vec2.null ()) in
		let bs = Array.map ForceDirectedLayout.body_of_pos fig.ps in

		for i=0 to 2000 do
			update_state n fs bs fig
		done;

		let x0, y0 = (10.,10.) in
		unsafe_reframe (10.,10.) fig.ps;
		unsafe_crop_width (800.-.2.*.x0) fig.ps;
		fig.h <- height fig.ps +. 2. *. y0;  
		
	    (* let x0 = 10. in *)
		(* unsafe_reframe (10., 10.) fig.ps; *)
		(* unsafe_crop_width (800.-.2.*.x0) fig.ps; *)
		
		Phylogram.write_svg_file collec fig svg_file

