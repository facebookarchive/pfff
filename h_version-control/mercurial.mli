
(* could also use a converter like maybe a git-hg if it exists *)

(* operations on a singular file *)

val annotate : 
  ?basedir:string -> Common.filename -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> Common.filename -> Common2.date_dmy
val annotate_raw : 
  ?basedir:string -> Common.filename -> string array
