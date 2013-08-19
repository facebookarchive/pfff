(*s: model_graph_code.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
(*e: Facebook copyright *)
(*e: model_graph_code.ml *)
open Common

module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build_uses_and_users_of_file g =

  (* we use the 'find_all' property of those hashes *)
  let huses = Hashtbl.create 101 in
  let husers = Hashtbl.create 101 in

  let halready = Hashtbl.create 101 in

  g +> G.iter_use_edges (fun n1 n2 ->
    try 
      let file1 = G.file_of_node n1 g in
      let file2 = G.file_of_node n2 g in
      if file1 <> file2 && not (Hashtbl.mem halready (file1, file2)) then begin
        Hashtbl.replace halready (file1, file2) true;
        Hashtbl.add huses file1 file2;
        Hashtbl.add husers file2 file1;
      end;
    with Not_found -> ()
  );

  Common2.hkeys huses +> List.map (fun k -> k, Hashtbl.find_all huses k),
  Common2.hkeys husers +> List.map (fun k -> k, Hashtbl.find_all husers k)

let build_entities_of_file g =

  (* we use the 'find_all' property of those hashes *)
  let h = Hashtbl.create 101 in

  g +> G.iter_nodes (fun n ->
    try 
      let info = G.nodeinfo n g in
      let file = info.G.pos.Parse_info.file in
      (* codemap use line numbers starting at 0 *)
      let line = info.G.pos.Parse_info.line - 1 in

      Hashtbl.add h file (line, n);
    with Not_found -> ()
  );
  Common2.hkeys h +> List.map (fun k ->
    let xs = Hashtbl.find_all h k in
    k, Common.sort_by_key_lowfirst xs
  )

