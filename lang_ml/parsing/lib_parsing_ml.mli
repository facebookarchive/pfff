
val find_ml_files_of_dir_or_files: 
  Common.path list -> Common.filename list
val find_cmt_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val ii_of_any: Ast_ml.any -> Ast_ml.info list
val min_max_ii_by_pos: Ast_ml.info list -> Ast_ml.info * Ast_ml.info

val is_function_body: Ast_ml.seq_expr -> bool
