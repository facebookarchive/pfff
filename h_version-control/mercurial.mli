open Common

(* could also use a converter like maybe a git-hg if it exists *)

val annotate : 
  ?basedir:string -> filename -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> filename -> Common2.date_dmy

val annotate_raw : 
  ?basedir:string -> filename -> string array
