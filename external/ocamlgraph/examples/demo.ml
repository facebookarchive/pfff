(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Graph.Pack.Digraph
(* to get undirected graphs, change to 
   open Graph.Pack.Graph
*)

let show = display_with_gv

let g = Rand.graph ~v:10 ~e:20 ()
let () = show g

let g' = complement g
let () = show g'

let g' = mirror g
let () = show g'

let g' = transitive_closure ~reflexive:true g
let () = show g'

(* Intersection and union *)

let g1 = create ()
let g2 = create ()

let v1 = V.create 1
let v2 = V.create 2
let v3 = V.create 3
let v4 = V.create 4
let v5 = V.create 5
let v6 = V.create 6
let v7 = V.create 7

let () =
  add_edge g1 v1 v2;
  add_edge g1 v2 v1;
  add_edge g1 v1 v3;
  add_edge g1 v2 v3;
  add_edge g1 v5 v3;
  add_edge g1 v6 v6;
  add_vertex g1 v4

let () =
  add_edge g2 v1 v2;
  add_edge g2 v2 v3;
  add_edge g2 v1 v4;
  add_edge g2 v3 v6;
  add_vertex g2 v7

let () = show g1
let () = show g2
let g' = intersect g1 g2
let () = show g'
let g' = union g1 g2
let () = show g'


(*
Local Variables: 
compile-command: "make -C .. bin/demo.opt"
End: 
*)
