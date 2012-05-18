(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module E = Database_code
module G = Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * A program can be seen as a hierarchy of entities
 * (directory/package/module/file/class/function/method/field/...)
 * linked to each other through different mechanisms
 * (import/reference/extend/implement/instantiate/call/access/...),
 * This module is the basis for 'codegraph', a tool to help
 * visualize code dependencies or code relationships. 
 * It provides the core data structure of codegraph, 
 * an (hyper)graph of all the entities in a program linked
 * either via a 'has-a' relation, which represent the
 * hierarchies, or 'use-a', which represent the dependencies.
 * 
 * This file could have been named dependency_code.ml or
 * relation_code.ml 
 * 
 * Is this yet another code database? For PHP we already have
 * database_php.ml, tags_php.ml, database_light_php.ml, 
 * and now even a prolog database, ... that's a lot of code database.
 * They all have things in common, but by focusing here on one thing,
 * by just having a single graph, it's then
 * easier to reason and implement certain features.
 * I could have probably done the DSM using database_php.ml
 * but it was not made for that. Here the graph is
 * the core and simplest data structure that is needed.
 * This graph also unifies many things. For instance there is no
 * special code to handle directories or files, they are
 * just considered regular entities like module or classes 
 * and can have sub-entities.
 * 
 * todo:
 *  - how to handle duplicate entities (e.g. we can have two different
 *    files have the same module name, or two functions with the same
 *    name but one in a library and the other in a script).
 *    prepend a ___number suffix?
 *    Or just have one node with multiple parents :)
 * 
 *  - change API to allow by default to automatically create nodes
 *    when create edges with unexisting nodes? After all graphviz
 *    allow to specify graphs like this, which shorten graph
 *    description significantly. Can still have a 
 *    add_edge_throw_exn_if_not_present for the cases where we
 *    want extra security.
 * 
 *  - maybe I can generate the light database and prolog database
 *    from this graph_code.ml.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = string * E.entity_kind

type edge =
  | Has
  (* todo? refine by having different cases? Use of `Call|`Extend|...? *)
  | Use

(* 
 * We use an imperative, directed, with no intermediate node-index, 
 * graph.
 * 
 * We use two different graphs because we need an efficient way to
 * go up in the hierarchy to increment cells in the dependency matrix
 * so it's better to separate the two usages.
 * 
 * note: file information are in readable path format in Dir and File
 * nodes.
 *)
type graph = {
  (* Actually should really be a tree, but we need convenient
   * access to the children or parent of a node, which are provided
   * by the graph API so let's reuse that.
   *)
  has: node G.graph;
  (* The source and target should be enough information to understand
   * the kind of use. For instance a class referencing another class
   * is an 'extends'. A class referencing an Interface is an 'implements'.
   *)
  use: node G.graph;
}

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let root = ".", E.Dir

(*****************************************************************************)
(* Graph construction *)
(*****************************************************************************)
let create () =
  { has = G.create ();
    use = G.create ();
  }

let add_node n g =
  if G.has_node n g.has
  then failwith "node already present";
  if G.has_node n g.use
  then failwith "node already present";

  G.add_vertex_if_not_present n g.has;
  G.add_vertex_if_not_present n g.use;
  ()

let add_edge (n1, n2) e g =
  match e with
  | Has -> G.add_edge n1 n2 g.has
  | Use -> G.add_edge n1 n2 g.use

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
let save g file =
  Common.write_value g file

let load file =
  Common.get_value file

(*****************************************************************************)
(* Graph access *)
(*****************************************************************************)

let has_node n g =
  G.has_node n g.has

let pred n e g =
  match e with
  | Has -> G.pred n g.has
  | Use -> G.pred n g.use

let succ n e g =
  match e with
  | Has -> G.succ n g.has
  | Use -> G.succ n g.use


let parent n g =
  let xs = G.pred n g.has in
  Common.list_to_single_or_exn xs

let parents n g =
  G.pred n g.has

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let display_with_gv g =
  G.display_with_gv g.has
