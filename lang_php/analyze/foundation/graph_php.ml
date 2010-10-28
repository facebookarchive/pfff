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

module Ast = Ast_php 

open Ast_php

module G = Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * An adapter over Database_php.database caller/callees assoc tables
 * using Common.graph (itself an adapter over ocamlgraph/) to have
 * access to complex graph algorithms (e.g. strongly-connected-components).
 * 
 * See also callgraph_php.ml and callees_of_id() in database_php.ml.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type id_graph = Entity_php.id G.graph

(*****************************************************************************)
(* Graph building *)
(*****************************************************************************)

let build_entities_graph ~all_ids ~successors = 
  let g = G.create () in

  all_ids +> List.iter (fun id -> g +> G.add_vertex_if_not_present id);
  all_ids +> List.iter (fun id ->
    let succ = successors id in
    succ +> List.iter (fun id2 ->
      g +> G.add_edge id id2
    )
  );
  g

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let display_with_gv g db =
  let tmpfile = Common.new_temp_file "graph_php" "dot" in
  G.print_graph_generic ~str_of_key:(fun id ->
    (* Db.complete_name_of_id id db *)
    raise Todo
  )

