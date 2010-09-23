(* Yoann Padioleau
 * 
 * Copyright (C) 2009 Yoann Padioleau
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
(* Types *)
(*****************************************************************************)

type versionid = 
  VersionId of string (* a SHA1 code in hexa, or RCS version *)
type author = 
  Author of string

(* could also add author *)
type line_annotation = versionid * author * Common.date_dmy

type commit_patch = (string list) (* header *) * Patch.patchinfo


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let goto_dir basedir = 
  if basedir = "" 
  then "" 
  else spf "cd \"%s\"; " basedir

let dummy_annotation = 
  VersionId "dummy", Author "dummy", (DMY (Day 1, Jan, Year 1977))


let s_of_versionid (VersionId s) = s

let parse_commit_patch xs = 
  try 
    let (before, x, after) = 
      Common.split_when (fun l -> l =~ "^diff.*") xs 
    in
    before, (Patch.parse_patch (x::after))
  with Not_found ->
    (* for merge commit, have this:
     * ["commit 6d27fd0f1eb56184011683530c224f90490f45c7";
     * "Merge: 6677406... a9e8879...";
     * "Author: pad <pad@facebook.com>"; "Date:   6 weeks ago"; "";
     * "    Merge branch 'master' of /home/pad/release/pfff-git"; ""]
     *)
    if xs +> List.exists (fun s -> s =~ ".*Merge.*")
    then xs, []
    else 
      failwith ("wrong commit:" ^ List.hd xs)


let special_vcs_dirs = [".git"; ".svn"; ".hg"; "CVS"; "_darcs"]
let filter_vcs_dir2 dir =
  (* dont care about VCS files *)
  let last = Filename.basename dir in
  not (List.mem last special_vcs_dirs)


let filter_vcs_dir x = 
  Common.profile_code "Lib_vcs.filter_dir" (fun () ->
    filter_vcs_dir2 x
  )
