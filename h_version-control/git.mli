(* filename below is assumed to be the path of the file relative to basedir *)

(* finding the repository *)
val is_git_repository: Common.dirname -> bool
val find_root_from_absolute_path: Common.filename -> Common.dirname

(* operations on a singular file *)

(* note that the array is 0-indexed but the first entry is a dummy value. *)
val annotate: 
  ?basedir:string -> ?use_cache:bool -> ?use_dash_C:bool ->
  Common.filename -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> Common.filename -> Common2.date_dmy
val annotate_raw: 
  ?basedir:string -> Common.filename -> string array

(* repository operations *)

val branches:
  basedir:string -> string list
val commits: 
  ?extra_args:string -> basedir:string -> unit -> 
  (Lib_vcs.versionid * string) list

val refactoring_commits:
  ?since:string -> ?threshold:int -> Common.dirname -> unit
val parse_skip_revs_file:
  Common.filename -> Lib_vcs.versionid list

val grep:
  basedir:string -> string -> Common.filename list
(* returns a temporary file containing the content of filename at versionid *)
val show:
  basedir:string -> Common.filename -> Lib_vcs.versionid -> Common.filename

(* commitids operations *)

val commit_of_relative_time: 
  basedir:string -> string (* e.g. "2 days ago" *) -> Lib_vcs.versionid

(* this will not include the old_id. It's ]old_id..recent_id] *)
val commits_between_commitids: 
  basedir:string ->
  old_id:Lib_vcs.versionid -> 
  recent_id:Lib_vcs.versionid -> 
  Lib_vcs.versionid list

(* single commit operation *)

val commit_info: 
  basedir:string -> Lib_vcs.versionid -> string list
val commit_summary: 
  basedir:string -> Lib_vcs.versionid -> string
val commit_raw_patch: 
  basedir:string -> Lib_vcs.versionid -> string list
val commit_patch: 
  basedir:string -> Lib_vcs.versionid -> Lib_vcs.commit_patch

val file_to_commits: 
  basedir:string -> Lib_vcs.versionid list -> 
  (Common.filename * (Lib_vcs.versionid * Patch.fileinfo) list) list

val files_involved_in_diff:
  basedir:string -> Lib_vcs.versionid -> 
  (Lib_vcs.file_commit_status * Common.filename) list

(* line level operation (preparing commits) *)
val apply_patch: basedir:string -> string list -> unit

val get_2_best_blamers_of_lines: 
  basedir:string -> 
  ?use_cache:bool ->
  ?is_valid_author:(string -> bool) ->
  ?skip_revs:Lib_vcs.versionid list ->
  Common.filename -> 
  int list (* lines *) ->
  string list (* 2, 1, or zero blamers *)

val max_date_of_lines: 
  basedir:string -> 
  ?use_cache:bool ->
  ?skip_revs:Lib_vcs.versionid list ->
  Common.filename -> int list (* lines *) ->
  Common2.date_dmy

(* misc operations *)
val clean_git_patch: Patch.patch_raw -> Patch.patch_raw

val ext_git_annot_cache: string
val cleanup_cache_files: Common.dirname -> unit

(* raise exception if the return code is not good *)
val exec_cmd: basedir:Common.dirname -> string -> unit
