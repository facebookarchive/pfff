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
 * This module is the basis for codegraph, a tool to help
 * visualize code dependencies or code relationships. 
 * It provides the core data structure of codegraph, 
 * an (hyper)graph of all the entities in a program linked
 * either via a 'has-a' relation, which represent the
 * hierarchies, or 'use-a', which represent the dependencies.
 * 
 * This file could have been named dependency_code.ml or
 * relation_code.ml 
 * 
 * Is this yet another code database? For PHP we have
 * database_php.ml, tags_php.ml, database_light_php.ml, 
 * and now even a prolog database, ... that's a lot of code database.
 * They all have things in common, but by focusing on one thing,
 * here by just having a single graph, it's
 * easier to reason and implement certain features.
 * I could have probably done the DSM using database_php.ml
 * but it was not made for that. Here the graph is
 * the core and simplest data structure that is needed.
 * This graph also unify many things. For instance there is no
 * special code to handle directories or files, they are
 * just considered regular entities like module or classes 
 * and can have sub-entities.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? how to handle duplicate entities? prepend a ___number suffix?
 * Or just have one node with multiple parents :)
 *)
type node = string * E.entity_kind

type edge =
  | Has
  (* todo? refine by having different cases? Use of `Call|`Extend|...? *)
  | Use


(* 
 * This is an imperative, directed, with no intermediate node index, 
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
let root = "/", E.Dir

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
(* Debugging *)
(*****************************************************************************)

let display_with_gv g =
  raise Todo
