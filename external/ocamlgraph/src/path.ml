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

(* $Id: path.ml,v 1.6 2005-07-18 07:10:35 filliatr Exp $ *)

module type WEIGHT = sig
  type label
  type t
  val weight : label -> t
  val compare : t -> t -> int
  val add : t -> t -> t
  val zero : t
end

module type G = sig
  type t 
  module V : Sig.COMPARABLE 
  module E : sig 
    type t 
    type label 
    val label : t -> label 
    val dst : t -> V.t 
  end 
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
end

module Dijkstra
  (G: G)
  (W: WEIGHT with type label = G.E.label) =
struct

  open G.E

  module H =  Hashtbl.Make(G.V)

  module Elt = struct
    type t = W.t * G.V.t * G.E.t list

    (* weights are compared first, and minimal weights come first in the
       queue *)	       
    let compare (w1,v1,_) (w2,v2,_) =
      let cw = W.compare w2 w1 in
      if cw != 0 then cw else G.V.compare v1 v2
  end

  module PQ = Heap.Imperative(Elt)

  let shortest_path g v1 v2 =
    let visited = H.create 97 in
    let dist = H.create 97 in
    let q = PQ.create 17 in
    let rec loop () = 
      if PQ.is_empty q then raise Not_found;
      let (w,v,p) = PQ.pop_maximum q in
      if G.V.compare v v2 = 0 then 
	List.rev p, w
      else begin
	if not (H.mem visited v) then begin
	  H.add visited v ();
	  G.iter_succ_e
	    (fun e -> 
	       let ev = dst e in
	       if not (H.mem visited ev) then begin
		 let dev = W.add w (W.weight (label e)) in
		 let improvement =
		   try W.compare dev (H.find dist ev) < 0 with Not_found -> true
		 in
		 if improvement then begin
		   H.replace dist ev dev;
		   PQ.add q (dev, ev, e :: p)
		 end
	       end)
	    g v
	end;
	loop ()
      end
    in
    PQ.add q (W.zero, v1, []);
    H.add dist v1 W.zero;
    loop ()

end



module Check 
  (G : 
    sig
      type t
      module V : Sig.COMPARABLE
      val iter_succ : (V.t -> unit) -> t -> V.t -> unit
    end) = 
struct

  module HV = Hashtbl.Make(G.V)
  module HVV = Hashtbl.Make(Util.HTProduct(G.V)(G.V))

  (* the cache contains the path tests already computed *)
  type path_checker = { cache : bool HVV.t; graph : G.t }

  let create g = { cache = HVV.create 97; graph = g }

  let check_path pc v1 v2 =
    try 
      HVV.find pc.cache (v1, v2)
    with Not_found -> 
      (* the path is not in cache; we check it with Dijkstra *)
      let visited = HV.create 97 in
      let q = Queue.create () in
      let rec loop () =
	if Queue.is_empty q then begin
	  HVV.add pc.cache (v1, v2) false;
	  false
	end else begin
	  let v = Queue.pop q in
	  HVV.add pc.cache (v1, v) true;
	  if G.V.compare v v2 = 0 then 
	    true
	  else begin
	    if not (HV.mem visited v) then begin
	      HV.add visited v ();
	      G.iter_succ (fun v' -> Queue.add v' q) pc.graph v
	    end;
	    loop ()
	  end
	end
      in
      Queue.add v1 q;
      loop ()

end
