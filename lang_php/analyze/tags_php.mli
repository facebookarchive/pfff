
val php_defs_of_files_or_dirs:
  ?verbose:bool ->
  Common.path list ->
  (Common.filename * Tags_file.tag list) list

val tags_of_ast: 
  Ast_php.program -> string array -> Tags_file.tag list
