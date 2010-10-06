
val find_ml_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val ii_of_toplevel: Ast_ml.toplevel -> Ast_ml.info list
val min_max_ii_by_pos: Ast_ml.info list -> Ast_ml.info * Ast_ml.info
