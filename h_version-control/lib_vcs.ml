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
module Date = Common2

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type versionid = 
  VersionId of string (* a SHA1 code in hexa, or RCS version *)
type author = 
  Author of string

(* could also add author *)
type line_annotation = versionid * author * Common2.date_dmy

type commit_patch = (string list) (* header *) * Patch.patchinfo

(* see man git-diff and the "diff-filter section" *)
type file_commit_status =
  | Added
  | Copied
  | Deleted
  | Modified
  | Renamed of 
      int (* probability of rename *) *
      Common.filename (* original filename *)
  | FileTypeChanged
  | Unmerged
  | Unknown
  | Broken

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let goto_dir basedir = 
  if basedir = "" 
  then "" 
  else spf "cd \"%s\"; " basedir

let dummy_annotation = 
  VersionId "dummy", Author "dummy", 
  (Date.DMY (Date.Day 1, Date.Jan, Date.Year 1977))

let s_of_versionid (VersionId s) = s

let parse_commit_patch xs = 
  try 
    let (before, x, after) = 
      Common2.split_when (fun l -> l =~ "^diff.*") xs 
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

(* See man git-diff and the "diff-filter section".
 * Could be in git.ml, but this look quite generic.
 *)
let parse_file_status s =
  match s with
  | _ when s=~ "\\([MADCTUXBR]\\)[ \t]+\\([^ \t]+\\)" ->
    let (status, name) = Common.matched2 s in
    (* coupling: if you add case below, don't forget to update the regexp 
     * above accordingly.
     *)
    (match status with
    (* git and mercurial *)
    | "A" -> Added
    | "M" -> Modified

    (* git only *)
    | "D" -> Deleted

    | "C" -> Copied
    | "T" -> FileTypeChanged
    | "U" -> Unmerged
    | "X" -> Unknown
    | "B" -> Broken

    (* mercurial *)
    | "R" -> Deleted
    (* todo: mercurial has also those flags but they should not appear
     *  when one look for the status of a change (e.g. hg status --change .)
     * '?': Mercurial doesn't know about this file, and has not been
     *      configured to ignore it
     * '!': Mercurial is tracking this file, but it's missing - perhaps 
     *      you've deleted it without telling Mercurial to stop tracking it.
     *)

    | _ -> failwith (spf "unknown file commit status: %s" status)
    ), name
  | _ when s =~ "R\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)" ->
      let (proba, src, target) = Common.matched3 s in
      Renamed (int_of_string proba, src), target
  | _ when s =~ "MM[ \t]+\\([^ \t]+\\)" ->
      let name = Common.matched1 s in
      (* todo? really that? look 61be1edbf71adbbb6bd07fe451b7639df72c004b in
       * facebook repo. Seems to be when modified in a merge-branch commit.
       *)
      Modified, name

  | _ -> failwith (spf "wrong format in file commit status: %s" s)
