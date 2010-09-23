(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2008                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: topological.ml,v 1.5 2004-02-20 14:37:41 signoles Exp $ i*)

module type G = sig
  type t
  module V : Sig.HASHABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val in_degree : t -> V.t -> int
end

module Make(G: G) = struct

  module H = Hashtbl.Make(G.V)

  let fold f g acc =
    let degree = H.create 997 in
    let todo = Queue.create () in
    let push x =
      H.remove degree x;
      Queue.push x todo
    in
    let rec walk acc = 
      if Queue.is_empty todo then
        (* let's find any node of minimal degree *)
	let min =
	  H.fold
	    (fun v d acc -> 
	       match acc with
	       | None -> Some (v, d)
	       | Some(_, min) -> if d < min then Some (v, d) else acc)
	    degree
	    None
	in
	match min with
	| None -> acc
	| Some(v, d) -> push v; walk acc
      else
	let v = Queue.pop todo in
	let acc = f v acc in
	G.iter_succ 
	  (fun x-> 
             try 
               let d = H.find degree x in
	       if d = 1 then push x else H.replace degree x (d-1)
             with Not_found -> 
	       (* [x] already visited *)
	       ())
	  g v; 
	walk acc
    in
    G.iter_vertex 
      (fun v -> 
	 let d = G.in_degree g v in 
	 if d = 0 then Queue.push v todo
	 else H.add degree v d)
      g;
    walk acc

  let iter f g = fold (fun v () -> f v) g ()

end
