(*s: lib_parsing_php.mli *)
val is_php_script: Common.filename -> bool
val find_php_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val ii_of_any: Ast_php.any -> Ast_php.info list
(*x: lib_parsing_php.mli *)
(* do via side effects *)
val abstract_position_info_toplevel: Ast_php.toplevel -> Ast_php.toplevel
val abstract_position_info_expr: Ast_php.expr -> Ast_php.expr
val abstract_position_info_lvalue: Ast_php.lvalue -> Ast_php.lvalue
val abstract_position_info_program: Ast_php.program -> Ast_php.program
(*x: lib_parsing_php.mli *)
val range_of_origin_ii: Ast_php.info list -> (int * int) option
val min_max_ii_by_pos: Ast_php.info list -> Ast_php.info * Ast_php.info

type match_format =
  (* ex: tests/misc/foo4.php:3
   *  foo(
   *   1,
   *   2);
   *)
  | Normal 
  (* ex: tests/misc/foo4.php:3: foo( *)
  | Emacs 
  (* ex: tests/misc/foo4.php:3: foo(1,2) *)
  | OneLine

val print_match: ?format:match_format -> Ast_php.info list -> unit
val print_warning_if_not_correctly_parsed: 
  Ast_php.program -> Common.filename -> unit

(*x: lib_parsing_php.mli *)
val get_all_funcalls_any         : Ast_php.any -> string list
val get_all_constant_strings_any : Ast_php.any -> string list
val get_all_funcvars_any         : Ast_php.any -> string (* dname *) list
val get_vars                     : Ast_php.any -> Ast_php.dname list
val get_static_vars              : Ast_php.any -> Ast_php.dname list
val get_returns                  : Ast_php.any -> Ast_php.expr list
val get_vars_assignements        : Ast_php.any -> (string * Ast_php.expr list) list

val top_statements_of_program: 
    Ast_php.program -> Ast_php.stmt list

(*e: lib_parsing_php.mli *)
