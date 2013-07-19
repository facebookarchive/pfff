(* Yoann Padioleau
 * 
 * Copyright (C) 2013 Facebook
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

module VC = Version_control

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Generic interface on top of Git and Mercurial.
 * Could also use first class module or just plain records, but classes
 * seems a good fit for this.
 *)
 
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

class type vcs = object
method basedir: string

method grep: string -> Common.filename list
method show: Common.filename -> Lib_vcs.versionid -> Common.filename
method files_involved_in_diff: Lib_vcs.versionid -> 
  (Lib_vcs.file_commit_status * Common.filename) list
end 

(*****************************************************************************)
(* Instances *)
(*****************************************************************************)

(* could define also classes instead of immediate objects *)
let git ~basedir = 
 (object 
   method basedir = basedir
   method grep s =
     Git.grep ~basedir s
   method show file commitid = 
     Git.show ~basedir file commitid
   method files_involved_in_diff commitid = 
     Git.files_involved_in_diff ~basedir commitid
  end : vcs)

let hg ~basedir =
 (object 
   method basedir = basedir
   method grep s =
     Mercurial.grep ~basedir s
   method show file commitid = 
     Mercurial.show ~basedir file commitid
   method files_involved_in_diff commitid = 
     Mercurial.files_involved_in_diff ~basedir commitid
  end : vcs)

(*****************************************************************************)
(* Builder *)
(*****************************************************************************)

(* infer from basedir *)
let mk_vcs ~basedir =
  match Version_control.detect_vcs_source_tree basedir with
  | Some VC.Git -> git ~basedir
  | Some VC.Mercurial -> hg ~basedir
  | Some x -> 
    failwith (spf "generic vcs interface not supported for %s"
                (VC.string_of_vcs_kind x))
  | None -> 
    failwith (spf "could not detect any VCS in directory %s" basedir)

