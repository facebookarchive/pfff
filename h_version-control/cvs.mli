open Common

(* could also use git-cvs to convert to git, but not always practical,
 * for instance would need get full CVS root for freebsd *)

val annotate : 
  ?basedir:string -> filename -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> filename -> Common2.date_dmy

val annotate_raw : 
  ?basedir:string -> filename -> string array
