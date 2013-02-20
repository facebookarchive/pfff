
type versionid = 
  VersionId of string (* a SHA1 code in hexa, or RCS version *)
type author = 
  Author of string

type line_annotation = 
  versionid * author * Common2.date_dmy

type commit_patch = (string list) (* header *) * Patch.patchinfo

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

(* helpers *)
val s_of_versionid: versionid -> string

(* generate a "cd xxx" *)
val goto_dir : Common.filename (* basedir *) -> string

val dummy_annotation : line_annotation

val parse_commit_patch: string list -> commit_patch
val parse_file_status: string -> file_commit_status * Common.filename

val filter_vcs_dir: Common.dirname -> bool
