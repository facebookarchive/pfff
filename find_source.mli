
val files_of_dir_or_files: 
  lang:string -> verbose:bool ->
  Common.path list -> Common.filename list

(* will manage optional skip list *)
val files_of_root: lang:string -> Common.dirname -> Common.filename list

val finder: string -> (Common.path list -> Common.filename list)
