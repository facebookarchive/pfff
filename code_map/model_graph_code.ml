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
open Common

module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build_deps_of_file g =

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
      (* old: let line = info.G.pos.Parse_info.line in *)
      Hashtbl.add h file n;
    with Not_found -> ()
  );
  Common2.hkeys h +> List.map (fun k ->
    let xs = Hashtbl.find_all h k in
    k, xs
  )

(* Codegraph does not currently handle very well header files. It's because
 * an header contain entities that are considered DUPE of their 
 * corresponding entity in the source file. Right now we skip such 
 * headers (e.g. .mli) in codegraph. The code below is here to
 * adjust codegraph deficiencies by artificially associate the
 * nodes in the source file to also the header file so one can
 * hover a function signature in a .mli and get its uses, users, etc.
 * 
 * ugly: fix that in codegraph instead?
 *)
let add_headers_files_entities_of_file root xs =
  let headers =
    xs +> Common.map_filter (fun (file, xs) ->
      let (d,b,e) = Common2.dbe_of_filename file in
      match e with
      | "ml" -> 
        let header_readable = Common2.filename_of_dbe (d,b,"mli") in
        let header = Filename.concat root header_readable in
        if Sys.file_exists header 
        (* todo: we add too many defs here, a mli can actually restrict
         * the set of exported functions, but because we use such
         * information mostly when hovering over entities in a .mli,
         * this should be fine.
         *)
        then Some (header_readable, xs)
        else None
      | _ -> None
    )
  in
  headers @ xs
(*e: model_graph_code.ml *)
