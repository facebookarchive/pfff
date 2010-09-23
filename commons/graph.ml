(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * There are multiple libraries for graphs in OCaml, incorporating each
 * different graph algorithms:
 * 
 *  - Ocamlgraph, by Filliatre, Signoles, and more. It has transitive closure,
 *    kruskal, floyd, topological sort, CFC, etc. Probably the best, but it is
 *    heavily functorized. I thought it was too complicated because of all
 *    those functors but they also provide an easy interface without functor
 *    in pack.mli and sig_pack.mli which makes it almost usable
 *    (see paper from jfla05 on ocamlgraph).
 * 
 *  - A small graph library in ocamldot by Trevor Jim to compute the
 *    transitive reduction of a graph, aka its kernel.
 * 
 *  - A small library in ocamldoc by Guesdon, who ported into ocamldoc
 *    the functionality of ocamldot, and apparently uses the opportunity 
 *    to rewrite too his own graph library. Has also the transitive
 *    reduction.
 * 
 *  - Camllib by jeannet ?
 * 
 * There are probably more, see the caml hump.
 *    
 * 
 * I have also developed a few graph libraries, but really just
 * to have a data type with successors/predecessors accessors:
 * 
 *  - common.ml type 'a graph. No algorithm, just builder/accessors.
 *  - ograph.ml object version of Common.graph, just the interface.
 *  - ograph2way.ml a generic version, inherit ograph.
 *  - ograph_extended.ml, implicit nodei = int for key.
 *  - ograph_simple.ml, key can be specified, for instance can be a string,
 *    so dont have to pass through the intermediate nodei for everything.
 * 
 * ograph_simple and ograph_extended and ograph2way shows that there is not
 * a single graph that can accomodate all needs while still being convenient.
 * ograph_extended is more generic, but you pay a little for that by
 * forcing the user to have this intermediate 'nodei'. The people 
 * from ocamlgraph have well realized that and made it possible
 * to have different graph inteface (imperative/pure, directed/undirected,
 * with/witout nodes, paramemtrized vertex or not, ...) and reuse
 * lots of code for the algorithm. Unfortunately, just like for the C++
 * STL, it comes at a price: lots of functors. The sig_pack.mli and pack.ml
 * tries to solve this pb, but they made some choices about what should
 * be the default that are not always good, and they do not allow
 * polymorphic nodes, which I think is quite useful (especially when
 * you want to display your graph with dot, you want to see the label
 * of the nodes, and not just integers. 
 * 
 * So This module is a small modification of ocamlgraph, to have 
 * more polymorphic graph with some defaults that makes sense most
 * of the time (Directed graph, Imperative, vertex ints with mapping
 * to node information), and which can use algorithms defined in 
 * other libraries by making some small converters from one representation
 * to the other (e.g. from my ograph_simple to ocamlgraph, and vice versa).
 * 
 * Note, ocamlgraph is really good and this file is useful
 * but for quick and dirty simple graph stuff, 
 * ograph_simple should be simpler (less dependencies). You can 
 * use it directly from common.cma. Then with the converter to ocamlgraph,
 * you can start with ograph_simple, and if in a few places you need
 * to use the graph algorithm provided by ocamlgraph or ocamldot, then
 * use the adapters.
 * 
 * 
 * 
 * I have included also in commons/ the small code from ocamldot/ocamldoc in:
 *  - ocamlextra/graph_ocamldot.ml 
 *  - ocamlextra/graph_ocamldoc.ml
 * 
 * Alternatives in other languages:
 *  - c++ GTL, graph template library
 *  - c++ ASTL, automata library
 * 
 * 
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* OG for ocamlgraph *)
module OG = Ocamlgraph.Pack.Digraph

(* TODO:
 *  sucks to have this intermediate module. Should copy paste
 *  pack.ml of ocamlgraph, make the code polymorphic, and incorporate
 *  the node info to int vertices. And also make a 'dot' that 
 *  can be parametrized by a str_of_node function, like in
 *  ograph_extended.
 * 
 *)

type 'key graph = {
  og: OG.t;

  key_of_vertex: (OG.V.t, 'key) Hashtbl.t;
  vertex_of_key: ('key, OG.V.t) Hashtbl.t;

  (* add node info also ? *)
  cnt: int ref;
}

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let create () = 
  { og = OG.create ();
    key_of_vertex = Hashtbl.create 101;
    vertex_of_key = Hashtbl.create 101;
    cnt = ref 0;
  }
let add_vertex_if_not_present key g = 
  if Hashtbl.mem g.vertex_of_key key
  then ()
  else begin
    incr g.cnt;
    let v = OG.V.create !(g.cnt) in 
    Hashtbl.add g.key_of_vertex v key;
    Hashtbl.add g.vertex_of_key key v;
    (* not necessary as add_edge automatically do that *)
    OG.add_vertex g.og v; 
  end
let vertex_of_key key g = 
  Hashtbl.find g.vertex_of_key key
let key_of_vertex v g = 
  Hashtbl.find g.key_of_vertex v

let add_edge k1 k2 g = 
  add_vertex_if_not_present k1 g;
  add_vertex_if_not_present k2 g;
  let vx = g +> vertex_of_key k1 in
  let vy = g +> vertex_of_key k2 in
  OG.add_edge g.og vx vy;
  ()

let shortest_path k1 k2 g = 
  let vx = g +> vertex_of_key k1 in
  let vy = g +> vertex_of_key k2 in

  let (edges, len) = OG.shortest_path g.og vx vy in
  let vertexes = 
    vx::(edges +> List.map (fun edge -> OG.E.dst edge))
  in
  vertexes |> List.map (fun v -> key_of_vertex v g)


let print_graph_generic ~str_of_key filename g = 
  Common.with_open_outfile filename (fun (pr,_) ->
    pr "digraph misc {\n" ;
    (* pr "size = \"10,10\";\n" ; *)

    g.og |> OG.iter_vertex (fun v -> 
      let k = key_of_vertex v g in
      pr (spf "%d [label=\"%s\"];\n" 
             (OG.V.label v)
             (str_of_key k));
    );

    g.og |> OG.iter_vertex (fun v -> 
      let succ = OG.succ g.og v in
      succ |> List.iter (fun v2 ->
        pr (spf "%d -> %d;\n" (OG.V.label v) (OG.V.label v2));
      )
    );
    pr "}\n" ;
    );
  Ograph_extended.launch_gv_cmd filename;
  ()

