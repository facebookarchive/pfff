
type versionid = 
  VersionId of string (* a SHA1 code in hexa, or RCS version *)
type author = 
  Author of string

type line_annotation = 
  versionid * author * Common.date_dmy

type commit_patch = (string list) (* header *) * Patch.patchinfo

type file_commit_status =
  | Modified
  | Deleted
  | Added

(* helpers *)
val s_of_versionid: versionid -> string

(* generate a "cd xxx" *)
val goto_dir : Common.filename (* basedir *) -> string

val dummy_annotation : line_annotation

val parse_commit_patch : string list -> commit_patch

val filter_vcs_dir: Common.dirname -> bool
