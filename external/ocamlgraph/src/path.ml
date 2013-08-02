(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
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
    val src : t -> V.t
    val dst : t -> V.t
  end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
  val nb_vertex : t -> int
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

(* The following module is a contribution of Yuto Takei (University of Tokyo) *)

module BellmanFord
  (G: G)
  (W: WEIGHT with type label = G.E.label) =
struct

  open G.E

  module H = Hashtbl.Make(G.V)

  exception NegativeCycle of G.E.t list

  let all_shortest_paths g vs =
    let dist = H.create 97 in
    H.add dist vs W.zero;
    let admissible = H.create 97 in
    let build_cycle_from x0 =
      let rec traverse_parent x ret =
	let e = H.find admissible x in
	let s = src e in
	if G.V.equal s x0 then e :: ret else traverse_parent s (e :: ret)
      in
      traverse_parent x0 []
    in
    let find_cycle x0 =
      let visited = H.create 97 in
      let rec visit x =
	if H.mem visited x then
	  build_cycle_from x
	else begin
	  H.add visited x ();
	  let e = H.find admissible x in
	  visit (src e)
	end
      in
      visit x0
    in
    let rec relax i =
      let update = G.fold_edges_e
        (fun e x ->
          let ev1 = src e in
          let ev2 = dst e in
          try begin
            let dev1 = H.find dist ev1 in
            let dev2 = W.add dev1 (W.weight (label e)) in
            let improvement =
              try W.compare dev2 (H.find dist ev2) < 0
              with Not_found -> true
            in
            if improvement then begin
              H.replace dist ev2 dev2;
	      H.replace admissible ev2 e;
              Some ev2
            end else x
          end with Not_found -> x) g None in
      match update with
      | Some x ->
        if i == G.nb_vertex g then raise (NegativeCycle (find_cycle x))
        else relax (i + 1)
      | None -> dist
    in
    relax 0

  let find_negative_cycle_from g vs =
    try let _ = all_shortest_paths g vs in raise Not_found
    with NegativeCycle l -> l


  module Comp = Components.Make(G)

  (* This is rather inefficient implementation. Indeed, for each
     strongly connected component, we run a full Bellman-Ford
     algorithm using one of its vertex as source, taking all edges
     into consideration.  Instead, we could limit ourselves to the
     edges of the component. *)
  let find_negative_cycle g =
    let rec iter = function
      | [] ->
          raise Not_found
      | (x :: _) :: cl ->
          begin try find_negative_cycle_from g x with Not_found -> iter cl end
      | [] :: _ ->
          assert false (* a component is not empty *)
    in
    iter (Comp.scc_list g)

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
