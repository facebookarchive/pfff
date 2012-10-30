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

module G = Graph_code
module E = Database_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Small wrapper around Graph_code_java to extract tags information.
 * 
 * Normally you should really use Intellij or Eclipse to navigate
 * a Java codebase, but a few things can be faster to do with Emacs.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let defs_of_dir_or_file ?(verbose=false) dir_or_file skip_list =
  let g = Graph_code_java.build ~verbose ~only_defs:true dir_or_file skip_list
  in

  (* use the multi-val-for-same-key propery of Hashtbl.add *)
  let h = Hashtbl.create 101 in
  let hmemo = Hashtbl.create 101 in

  g +> G.iter_nodes (fun (str, kind) ->
    try 
    let nodeinfo = g +> G.nodeinfo (str, kind) in
    let parse_info = nodeinfo.G.pos in
    let info = { PI.
       token = PI.OriginTok parse_info;
       comments = ();
       transfo = PI.NoTransfo;
    }
    in
    (* wants the fully qualified tag, foo.Bar, not just Bar *)
    let info = PI.rewrap_str str info in

    let file = PI.file_of_info info in
    let filelines = 
      Common.memoized hmemo file (fun () ->
        Common.cat_array file
      )
    in

    let tag = Tags_file.tag_of_info filelines info kind in
    Hashtbl.add h file tag
    with Not_found ->
      (match kind with
      | E.Package | E.File | E.Dir | E.TopStmts -> ()
      | _ -> pr2 (spf "PB, nodeinfo not found for %s" str);
      )
  );
  let keys = Common.hkeys h in
  keys +> List.map (fun k -> k, Hashtbl.find_all h k)

  
