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
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for ML compiled objects. See graph_code.ml
 * and main_codegraph.ml for more information.
 * 
 * As opposed to lang_ml/analyze/graph_code_ml.ml, no need for:
 *  ???
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  current: Graph_code.node;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let parse ~show_parse_error file =
  try 
    Common.memoized _hmemo file (fun () ->
      Cmt_format.read_cmt file
    )
  with exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file 
                  (Common.exn_to_s exn));
    raise exn

let find_source_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  +> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | FT.Obj "cmt" -> true
    | _ -> false
  ) +> Common.sort

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir_or_file skip_list =
  let root = Common.realpath dir_or_file in
