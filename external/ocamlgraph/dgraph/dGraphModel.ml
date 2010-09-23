(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

(* This graph model is for now immutable, no adding or removing nodes. *)

open Ocamlgraph
open XDot
open Printf

exception DotError of string

(* ABSTRACT CLASS *)

class type ['vertex, 'edge, 'cluster] abstract_model = object
  method iter_edges : ('vertex -> 'vertex -> unit) -> unit
  method iter_edges_e : ('edge -> unit) -> unit
  method iter_pred : ('vertex -> unit) -> 'vertex -> unit
  method iter_pred_e : ('edge -> unit) -> 'vertex -> unit
  method iter_succ : ('vertex -> unit) -> 'vertex -> unit
  method iter_succ_e : ('edge -> unit) -> 'vertex -> unit
  method iter_vertex : ('vertex -> unit) -> unit
  method iter_clusters : ('cluster -> unit) -> unit

  (** Membership functions *)
  method find_edge : 'vertex -> 'vertex -> 'edge
  method mem_edge : 'vertex -> 'vertex -> bool
  method mem_edge_e : 'edge -> bool
  method mem_vertex : 'vertex -> bool
  method src : 'edge -> 'vertex
  method dst : 'edge -> 'vertex

  (** Dot layout *)
  method bounding_box : bounding_box
  method get_edge_layout : 'edge -> edge_layout
  method get_vertex_layout : 'vertex -> node_layout
  method get_cluster_layout : 'cluster -> cluster_layout
end

(* BUILDING A MODEL WITH AN OCAML GRAPH *)

module Make(G : Graphviz.GraphWithDotAttrs) = struct

  type cluster = string
      
  class model layout g : [G.vertex, G.edge, cluster] abstract_model = object

    (* Iterators *)
    method iter_edges f = G.iter_edges f g
    method iter_edges_e f = G.iter_edges_e f g
    method iter_pred f v = G.iter_pred f g v
    method iter_pred_e f v = G.iter_pred_e f g v
    method iter_succ f = G.iter_succ f g
    method iter_succ_e f = G.iter_succ_e f g
    method iter_vertex f = G.iter_vertex f g
    method iter_clusters f = 
      Hashtbl.iter (fun k v -> f k) layout.XDot.cluster_layouts

    (* Membership functions *)
    method find_edge = G.find_edge g
    method mem_edge = G.mem_edge g
    method mem_edge_e = G.mem_edge_e g
    method mem_vertex = G.mem_vertex g
    method src = G.E.src
    method dst = G.E.dst
      
    (* Layout *)
    method bounding_box = layout.XDot.bbox
    method get_vertex_layout v = (*Hashtbl.find layout.XDot.vertex_layouts*)
      try Hashtbl.find layout.XDot.vertex_layouts v
      with Not_found -> 
	failwith ("Could not find layout of vertex named " ^ G.vertex_name v)
    method get_edge_layout = Hashtbl.find layout.XDot.edge_layouts
    method get_cluster_layout = Hashtbl.find layout.XDot.cluster_layouts

  end
    
  let from_graph ?(cmd="dot") ?(tmp_name = "dgraph") g =
    (* Output dot file *)
    let module DumpDot = Graphviz.Dot(G) in
    let dot_file, out = Filename.open_temp_file tmp_name ".dot" in
    DumpDot.output_graph out g;
    close_out out;    
    (* Get layout from dot file *)
    let module X = XDot.Make(G) in
    let layout = X.layout_of_dot ~cmd ~dot_file g in
    let model = new model layout g in
    Sys.remove dot_file;
    model

end

(* BUILDING A MODEL WITH A DOT FILE *)

(* Here we build a model from a graph where vertices and edges
   are labeled with xdot layouts *)

module Vertex = struct
  type t = XDot.node_layout
end

module Edge = struct
  type t = XDot.edge_layout
  let default = XDot.mk_edge_layout
    ~draw:[] ~ldraw:[] ~hdraw:[] ~tdraw:[] ~hldraw:[] ~tldraw:[]
  let compare = compare
end

module DotG = Imperative.Digraph.AbstractLabeled(Vertex)(Edge)
module DotB = Builder.I(DotG)

type cluster = string
type dotg_model = (DotG.vertex, DotG.edge, cluster) abstract_model

module DotParser =
  Dot.Parse
    (DotB)
    (struct 
       (* Read the attributes of a node *)
       let node = XDot.read_node_layout

       (* Read edge attributes *)
       let edge = XDot.read_edge_layout

     end)

module DotModel = struct
  type cluster = string
  type clusters_hash = (cluster, Ocamlgraph.Dot_ast.attr list) Hashtbl.t
  class model g clusters_hash bounding_box 
    : [DotG.vertex, DotG.edge, cluster] abstract_model 
    =
  object
    (* Iterators *)
    method iter_edges f = DotG.iter_edges f g
    method iter_edges_e f = DotG.iter_edges_e f g
    method iter_pred f v = DotG.iter_pred f g v
    method iter_pred_e f v = DotG.iter_pred_e f g v
    method iter_succ f = DotG.iter_succ f g
    method iter_succ_e f = DotG.iter_succ_e f g 
    method iter_vertex f = DotG.iter_vertex f g
    method iter_clusters f = Hashtbl.iter (fun k _ -> f k) clusters_hash

    (* Membership functions *)
    method find_edge = DotG.find_edge g
    method mem_edge = DotG.mem_edge g
    method mem_edge_e = DotG.mem_edge_e g
    method mem_vertex = DotG.mem_vertex g
    method src = DotG.E.src
    method dst = DotG.E.dst

    (* Layout *)
    method bounding_box = bounding_box
    method get_vertex_layout = DotG.V.label
    method get_edge_layout = DotG.E.label
    method get_cluster_layout c =
      let attrs = Hashtbl.find clusters_hash c in
      XDot.read_cluster_layout attrs
  end

  let model = new model
end

(* Runs graphviz, parses the graph and instantiates the model *)
let read_dot ?(cmd="dot") ~dot_file =
  let basename = try Filename.chop_extension dot_file 
                 with Invalid_argument _ -> dot_file in
  let xdot_file = basename ^ ".xdot" in
  let dot_cmd = Printf.sprintf "%s -Txdot %s > %s" cmd dot_file xdot_file in

  (* Run graphviz *)
  match Sys.command dot_cmd with
    | 0 -> begin
	let graph, bb, clusters_hash =
	  DotParser.parse_bounding_box_and_clusters xdot_file in
	DotModel.model graph clusters_hash (XDot.read_bounding_box bb)
      end
    | _ -> raise (DotError "Error during dot execution")

(* Does not run graphviz
   Parses a graph from an xdot file and instantiates the model
*)
let read_xdot ~xdot_file =
  let graph, bb, clusters_hash =
    DotParser.parse_bounding_box_and_clusters xdot_file in
  DotModel.model graph clusters_hash (XDot.read_bounding_box bb)
