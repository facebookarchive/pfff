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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Programs can be seen as hierarchies of entities 
 * (package/module/class/function/method/field/...)
 * linked to each other through different mechanisms 
 * (import/reference/extend/instantiate/call/access/...), 
 * This module is the basis for codegraph, a tool to help
 * visualize code dependencies or code relationships. 
 * It provides the core data structure of codegraph, 
 * an (hyper)graph of all the entities in a program linked
 * either via a 'has-a' relation, which represent the
 * hierarchies, or 'use-a', which represent the dependencies.
 * 
 * This file could have been named dependency_code.ml or
 * relation_code.ml 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? how to handle duplicate entities? prepend a ___number suffix? *)
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
  (* Actually a tree, but we need convenient access to the children or
   * parent of a node, which are provided by the graph API.
   *)
  has: unit;
  (* The source and target should be enough information to understand
   * the kind of use. For instance a class referencing another class
   * is an 'extends'. A class referencing an Interface is an 'implements'.
   *)
  use: unit;
}

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let root = "/", E.Dir

(*****************************************************************************)
(* Graph construction *)
(*****************************************************************************)
let create () =
  raise Todo


let add_node n g =
  raise Todo

let add_edge (n1, n2) e g =
  raise Todo


(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let display_with_gv g =
  raise Todo
