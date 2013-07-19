(* filename below is assumed to be the path of the file relative to basedir *)

(* operations on a singular file *)

val annotate : 
  ?basedir:string -> Common.filename -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> Common.filename -> Common2.date_dmy
val annotate_raw : 
  ?basedir:string -> Common.filename -> string array

(* repository operations *)

val grep:
  basedir:string -> string -> Common.filename list
(* returns a temporary file containing the content of filename at versionid *)
val show:
  basedir:string -> Common.filename -> Lib_vcs.versionid -> Common.filename

val files_involved_in_diff:
  basedir:string -> Lib_vcs.versionid -> 
  (Lib_vcs.file_commit_status * Common.filename) list
