(*s: lib_parsing_php.mli *)
val is_php_filename: Common.filename -> bool

(* this will open the file and look possible for a #!/usr/bin/env php *)
val is_php_file: Common.filename -> bool
val is_php_script: Common.filename -> bool

val find_source_files_of_dir_or_files: 
  ?verbose:bool ->
  ?include_hack:bool ->
  Common.path list -> Common.filename list

(*x: lib_parsing_php.mli *)
(* returns only origin tokens, filter fake tokens *)
val ii_of_any: Ast_php.any -> Ast_php.tok list
(*x: lib_parsing_php.mli *)
(* do via side effects *)
val abstract_position_info_any: Ast_php.any -> Ast_php.any

(*x: lib_parsing_php.mli *)
val range_of_origin_ii: Ast_php.tok list -> (int * int) option

(*x: lib_parsing_php.mli *)
val get_funcalls_any         : Ast_php.any -> string list
val get_constant_strings_any : Ast_php.any -> string list
val get_vars_any              : Ast_php.any -> Ast_php.dname list
val get_static_vars_any       : Ast_php.any -> Ast_php.dname list
val get_returns_any           : Ast_php.any -> Ast_php.expr list
val get_vars_assignements_any : Ast_php.any -> (string * Ast_php.expr list) list

val top_statements_of_program: 
  Ast_php.program -> Ast_php.stmt list

val functions_methods_or_topstms_of_program:
  Ast_php.program -> 
  (Ast_php.func_def list * Ast_php.method_def list * Ast_php.stmt list list) 

(*e: lib_parsing_php.mli *)
