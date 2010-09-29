
val is_js_script: Common.filename -> bool
val find_js_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val ii_of_toplevel: Ast_js.toplevel -> Ast_js.info list
val ii_of_expr: Ast_js.expr -> Ast_js.info list

val min_max_ii_by_pos: Ast_js.info list -> Ast_js.info * Ast_js.info
