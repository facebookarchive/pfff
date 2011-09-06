open ExtLib
open Tree
open Phylogram
open BarnesHut
open Vec2

open Printf

let repulse_const = ref 1.
let drag_const = ref 4.
let spring_const = ref 1.

let set_drag_const x = drag_const := x
let set_spring_const x = spring_const := x
let set_repulse_const x = repulse_const := x

let body_of_pos ?(v = Vec2.null ()) p =
	{ p = p;
	  v = v }

let do_calc_drag_forces fs bs =
	for i=0 to Array.length fs - 1 do
		let f = fs.(i) in
		let b = bs.(i) in
		f.x <- f.x -. !drag_const *. abs_float(b.v.x) *. b.v.x;
		f.y <- f.y -. !drag_const *. abs_float(b.v.y) *. b.v.y
	done

let do_calc_spring_forces fs bs tree_fig =
	let parents = tree_fig.tree.parents in
	let dmat = tree_fig.tree.dist_mat in
	for i=1 to Array.length parents - 1 do
		let p = parents.(i) in
		let d = float_of_int (DistMat.get dmat p i + 1) in
		let dx = bs.(p).p.x -. bs.(i).p.x in
		let dy = bs.(p).p.y -. bs.(i).p.y in
		let r = sqrt (square dx +. square dy) in
		let factor = !spring_const *. (r -. d) in
		let fx = factor *. dx /. r in
		let fy = factor *. dy /. r in
		let f = fs.(i) in
		let f' = fs.(p) in
		f.x <- f.x +. fx;
		f.y <- f.y +. fy;
		f'.x <- f'.x -. fx;
		f'.y <- f'.y -. fy
	done

let (+|) = Vec2.add
let (-|) = Vec2.sub

let do_calc_spring_forces2 fs bs tree_fig =
	let adj_mat = tree_fig.tree.adj_mat in
	let dmat = tree_fig.tree.dist_mat in
	for i = 0 to Array.length adj_mat - 1 do
		for j=0 to i-1 do
			if DistMat.get adj_mat i j then
				let diff = bs.(j).p -| bs.(i).p in
				let r = Vec2.norm diff in
				let dist = float_of_int (DistMat.get dmat i j) in
				let f = Vec2.scal_mul
					(!spring_const *. (r -. dist) /. r) diff in
				fs.(i) <- fs.(i) +| f;
				fs.(j) <- fs.(j) -| f
		done
	done

let do_calc_forces fs bs tree_fig =
	let tree = BarnesHut.tree_of_bodies (Array.to_list bs) in
	BarnesHut.do_calc_forces fs (!repulse_const) bs tree;
	do_calc_drag_forces fs bs;
	do_calc_spring_forces fs bs tree_fig;
	()

let do_calc_forces_n2 fs bs tree_fig =
	for i=0 to Array.length fs - 1 do
		for j=0 to i-1 do
			let b = bs.(i) in
			let b' = bs.(j) in
			do_add_force_on fs.(j) !repulse_const b'.p b.p 1.;
			do_add_force_on fs.(i) !repulse_const b.p b'.p 1.;
		done;
	done;
	do_calc_drag_forces fs bs;
	do_calc_spring_forces fs bs tree_fig;
	()

(** Classic O(n^2) algorithm, debug purposes *)
(*val do_calc_forces : Vec2.t array -> body array -> unit*)
