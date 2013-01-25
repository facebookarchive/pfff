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
  | DirElement of Common.dirname
  | SkipErrorsDir of Common.dirname

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
    | _ when s =~ "^dir:[ ]*\\([^ ]+\\)" -> Dir (Common.matched1 s)
    | _ when s =~ "^skip_errors_dir:[ ]*\\([^ ]+\\)" -> 
        SkipErrorsDir (Common.matched1 s)
    | _ when s =~ "^file:[ ]*\\([^ ]+\\)" -> File (Common.matched1 s)
    | _ -> failwith ("wrong line format in skip file: " ^ s)
  )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: say when skipped stuff? *)
let filter_files skip_list root xs =
  let skip_files =
    skip_list +> Common.map_filter (function
    | File s -> Some s
    | _ -> None
    ) +> Common.hashset_of_list
  in
  let skip_dirs =
    skip_list +> Common.map_filter (function
    | Dir s -> Some s
    | _ -> None
    ) 
  in
  xs +> Common.exclude (fun file ->
    let readable = Common.filename_without_leading_path root file in
    (Hashtbl.mem skip_files readable) ||
    (skip_dirs +> List.exists (fun dir -> readable =~ (dir ^ ".*")))
  )

let build_filter_errors_file skip_list =
  let skip_dirs = 
    skip_list +> Common.map_filter (function
    | SkipErrorsDir dir -> Some dir
    | _ -> None
    )
  in
  (fun readable ->
    skip_dirs +> List.exists (fun dir -> readable =~ ("^" ^ dir))
  )

let reorder_files_skip_errors_last skip_list root xs =
  let is_file_want_to_skip_error = build_filter_errors_file skip_list in
  let (skip_errors, ok) = 
    xs +> List.partition (fun file ->
      let readable = Common.filename_without_leading_path root file in
      is_file_want_to_skip_error readable
    )
  in
  ok ++ skip_errors
