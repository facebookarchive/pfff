(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

module G = Graph_code
module E = Database_code

module Set = Set_poly

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type class_hierarchy = Graph_code.node Graph.graph

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* if B extends A then will have a node from A to B (children relation) *)
let class_hierarchy g =
  let dag = Graph.create () in
  
  g +> G.iter_nodes (fun n1 ->
    (match snd n1 with
    | E.Class _ ->
        dag +> Graph.add_vertex_if_not_present n1;

        (* explore if its extends/implements/uses another class/interf/trait *)
        let succ = g +> G.succ n1 G.Use in
        succ +> List.iter (fun n2 ->
          (match snd n2 with
          | E.Class _ ->
              dag +> Graph.add_vertex_if_not_present n2;
              (* from parent to children *)
              dag +> Graph.add_edge n2 n1
          | _ -> 
              failwith (spf "class_hierarchy: impossible edge %s --> %s"
                          (G.string_of_node n1) (G.string_of_node n2))
          )
        )
    | _ -> ()
  )
  );
  dag


let toplevel_methods g dag =
  let start = Graph.entry_nodes dag in

  let env = Set.empty in
  let htoplevels = Hashtbl.create 101 in

  let rec aux env n = 

    let methods_here =
      G.children n g +> Common.map_filter (fun n2 ->
          match snd n2 with
          | E.Method _ -> 
              let xs = Common.split "\\." (fst n2) in
              let method_str = Common2.list_last xs in
              let info = G.nodeinfo n2 g in
              let props = info.G.props in
              let privacy = props +> Common.find_some (function
                | E.Privacy p -> Some p
                | _ -> None
              )
              in
              Some (method_str, privacy, n2)
          | _ -> None
      )
    in
    methods_here +> List.iter (fun (s, priv, n2) ->
      if Set.mem s env
      then ()
      else
        (* We care only about toplevel public methods. Private or protected
         * methods can be used only via $this-> and so should be resolved.
         * Only calls like $o->foo() are unresolved and those methods
         * must be public methods.
         *)
        if priv = E.Public
        then Hashtbl.add htoplevels s n2
        else ()
    );
    let children_classes = Graph.succ n dag in
    let env = methods_here +> List.fold_left (fun acc (s, _p, _) ->
        (* todo? what if public overriding a private? *)
        Set.add s acc
    ) env
    in
    children_classes +> List.iter (aux env)
  in
  start +> List.iter (aux env);
  htoplevels
