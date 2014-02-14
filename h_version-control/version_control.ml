(* Yoann Padioleau
 * 
 * Copyright (C) 2009 Yoann Padioleau
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

type vcs_kind = Git | CVS | Mercurial | Subversion

let string_of_vcs_kind k =
  match k with
  | Git -> "git"
  | CVS -> "cvs"
  | Mercurial -> "hg"
  | Subversion -> "svn"

let tool_of_vcs_kind k =
  match k with
  | Git -> "git"
  | CVS -> "cvs"
  | Mercurial -> "hg"
  | Subversion -> "svn"


let detect_vcs_source_tree dir = 
  match () with
  | _ when Sys.file_exists (Filename.concat dir ".git") -> Some Git
  | _ when Sys.file_exists (Filename.concat dir "CVS") -> Some CVS
  | _ when Sys.file_exists (Filename.concat dir ".hg") -> Some Mercurial
  | _ when Sys.file_exists (Filename.concat dir ".svn") -> Some Subversion
  | _ -> None
