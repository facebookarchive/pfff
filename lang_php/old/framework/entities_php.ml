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

module CG = Callgraph_php 

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)


(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* we reuse Callgraph type but in spirit we could define another one as
 * we use those trees also for situations not related to callgraphs such 
 * as when we build the list via a tree of all entities in a file.
 *)
type idtree = Callgraph_php.idtree


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let first_id_in_tree tree = 
  let (Common.NodeRef (n, xs)) = tree in
  n.CG.id

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let node_of_id id idname = 
  { CG.id = id;
    name = idname id;
    extra = None;

    confidence = CG.no_info_confidence;
    gray = false;
(*
    extrabis = "";
    match_result = None;
*)
  }
  
(* ---------------------------------------------------------------------- *)
let tree_of_ids ?(sort=true) ids idname = 
  match ids with
  | [] -> failwith "tree_of_ids: empty"
  | id_head::id_rest -> 
      let nodes = ids +> List.map (fun id -> node_of_id id idname) in
      let nodes = 
        if sort then
          nodes +> List.sort (fun a b -> 
            compare a.CG.name b.CG.name
          ) 
        else nodes
      in
      let children = nodes +> List.map (fun n -> NodeRef (n, ref [])) in
      NodeRef
      (node_of_id id_head idname, ref children)


