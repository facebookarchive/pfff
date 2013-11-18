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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

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
          dag +> Graph.add_vertex_if_not_present n2;
          (match snd n2 with
          | E.Class _ ->
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

