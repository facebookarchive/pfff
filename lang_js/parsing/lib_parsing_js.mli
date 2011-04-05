
val is_js_script: Common.filename -> bool
val find_js_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val ii_of_any: Ast_js.any -> Ast_js.info list

val min_max_ii_by_pos: Ast_js.info list -> Ast_js.info * Ast_js.info
