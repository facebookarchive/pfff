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
(* 
 * Generating a (light) database from a graph_code.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
  
let db_of_graph_code root g =

  (* todo: if at some point we want to also leverage the 
   * cross entity functionality of Database_code, e.g.
   * its e_good_examples_of_use field, then we need to
   * do something a bit different here and map
   * nodes to indices first.
   *)
  let res = ref [] in

  (* opti: using G.parent and check if G.not_found is slow *)
  let hnot_found = G.all_children G.not_found g +> Common.hashset_of_list in

  (* opti: using G.pred is super slow *)
  let use_pred = G.mk_eff_use_pred g in
  
  g +> G.iter_nodes (fun node ->
    let (s, kind) = node in
    match kind with
    | E.Function | E.Class _ | E.Constant | E.Global | E.Type | E.Exception
    | E.Constructor | E.Field | E.Method _ | E.ClassConstant 
    | E.Macro
      ->
      if Hashtbl.mem hnot_found node
      then ()
      else begin
        let nodeinfo = 
          try 
            G.nodeinfo node g 
          with Not_found ->
            failwith (spf "No nodeinfo for %s" (G.string_of_node node))
        in
        let pos = nodeinfo.G.pos in
        let file = pos.Parse_info.file in

      (* select users that are outside! that are not in the same file *)
        let pred = use_pred node in
        let extern = pred +> List.filter (fun n ->
          try
            let nodeinfo = G.nodeinfo n g in
            let pos = nodeinfo.G.pos in
            let file2 = pos.Parse_info.file in
            file <> file2
          with
            Not_found -> false
        ) 
        in
        let nb_users = List.length extern in

        let xs = Common.split "\\." s in
        let e = { Database_code.
                  e_kind = kind;
                  e_name = Common2.list_last xs;
                  e_fullname = s;
                  e_file = pos.Parse_info.file;
                  e_pos = { Common2.
                            l = pos.Parse_info.line;
                            c = pos.Parse_info.column;
                          };
                  e_number_external_users = nb_users;
        (* todo *)
                  e_good_examples_of_use = [];
                  e_properties = [];
                }
        in
        Common.push2 e res
      end

    | E.TopStmts 
    | E.Module | E.Package
    | E.Prototype | E.GlobalExtern
    | E.Other _
    | E.File | E.Dir | E.MultiDirs
      -> ()
  );
  
  let arr = Array.of_list !res in
  
  { Database_code.
    root = root;
    dirs = [];
    files = [];
    entities = arr;
  }

