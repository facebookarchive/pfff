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

open Ast_cpp

module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

module V = Visitor_cpp

module FT = File_type

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  +> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | FT.PL (FT.C ("l" | "y")) -> false
    | FT.PL (FT.C _ | FT.Cplusplus _ | FT.ObjectiveC _) ->
        true
    | _ -> false

  ) +> Common.sort

(*****************************************************************************)
(* ii_of_any *)
(*****************************************************************************)

let ii_of_any any =
  let globals = ref [] in
  let visitor = V.mk_visitor { V.default_visitor with
    V.kinfo = (fun (k,_) i -> Common.push2 i globals)
  }
  in
  visitor any;
  List.rev !globals


