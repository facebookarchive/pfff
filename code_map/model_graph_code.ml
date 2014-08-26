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
module E = Entity_code
module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let is_prefix prefix str =
  (* todo: have better than that? *)
  try 
    String.sub str 0 (String.length prefix) =$= prefix
  with Invalid_argument _ -> false
  

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let build_filedeps_of_dir_or_file g =
  (* we use the 'find_all' property of those hashes *)
  let huses = Hashtbl.create 101 in
  let husers = Hashtbl.create 101 in

  let halready = Hashtbl.create 101 in

  g +> G.iter_use_edges (fun n1 n2 ->
    try 
      let file1 = G.file_of_node n1 g in
      let file2 = G.file_of_node n2 g in
      (* file to file deps *)
      if file1 <> file2 && not (Hashtbl.mem halready (file1, file2)) then begin
        Hashtbl.replace halready (file1, file2) true;
        Hashtbl.add huses (file1, E.File) file2;
        Hashtbl.add husers (file2, E.File) file1;
      end;
      (* dir to file deps *)
      (* e.g. if a/b/foo.c -> a/c/bar.c then need to add
       * a/b -> a/c/bar.c, but not a/ -> a/c/bar.c cos of is_prefix
       * a/c <- a/b/foo.c, but not a/ <- a/b/foo.c cos of is_prefix
       *)
      let dirs_n1 = Common2.inits_of_relative_dir file1 in
      let dirs_n2 = Common2.inits_of_relative_dir file2 in
      dirs_n1 +> List.iter (fun dir ->
        if not (is_prefix dir file2)
        then Hashtbl.add huses (dir, E.Dir) file2;
      );
      dirs_n2 +> List.iter (fun dir ->
        if not (is_prefix dir file1)
        then Hashtbl.add husers (dir, E.Dir) file1;
      );
        
    with Not_found -> ()
  );
  let hres = Hashtbl.create 101 in
  let keys = Common2.union_set (Common2.hkeys huses) (Common2.hkeys husers) in
  keys +> List.iter (fun k ->
    let uses = try Hashtbl.find_all huses k with Not_found -> [] in
    let users = try Hashtbl.find_all husers k with Not_found -> [] in
    (* todo: have to do uniq? if add in hash multiple times with same value,
     * then get multiple bindings?
     *)
    Hashtbl.add hres k (uses, users)
  );
  hres

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
      let (d,b,e) = Common2.dbe_of_filename_noext_ok file in
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

let node_of_entity e g =
  let fullname = 
    match e.Db.e_fullname with
    | "" -> e.Db.e_name
    | s -> s
  in 
  let node = (fullname, e.Db.e_kind) in
  if G.has_node node g
  then Some node
  else None
(*e: model_graph_code.ml *)
