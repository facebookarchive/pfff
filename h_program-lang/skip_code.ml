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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * It is often useful to skip certain parts of a codebase. Large codebase
 * often contains special code that can not be parsed, that contains
 * dependencies that should not exist, old code that we don't want
 * to analyze, etc.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the filename are in readable path format *)
type skip =
  (* mostly to avoid parsing errors messages *)
  | Dir of Common.dirname
  | File of Common.filename

  (* mostly for graph_code *)
  | Edge of string * string

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
let load file =
  Common.cat file 
  +> Common.exclude (fun s -> 
    s =~ "#.*" || s =~ "^[ \t]*$"
  )
  +> List.map (fun s ->
    match s with
    | _ when s =~ "dir:[ ]*\\([^ ]+\\)" -> Dir (Common.matched1 s)
    | _ when s =~ "file:[ ]*\\([^ ]+\\)" -> File (Common.matched1 s)
    | _ when s =~ "edge:[ ]*\\([^ -]+\\)[ ]*-->[ ]*\\([^ -]+\\)" ->
        let (s1, s2) = Common.matched2 s in
        Edge (s1, s2)
    | _ -> failwith ("wrong line format in skip file: " ^ s)
  )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_files ?(verbose=false) skip_list root xs =

  (* todo: 
   *  - dir 
   *  - say when skipped stuff?
   *)

  let skip_files =
    skip_list +> Common.map_filter (function
    | File s -> Some s
    | _ -> None
    ) +> Common.hashset_of_list
  in
  xs +> List.filter (fun file ->
    let readable = Common.filename_without_leading_path root file in
    not (Hashtbl.mem skip_files readable)
  )
