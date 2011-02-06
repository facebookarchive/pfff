(*s: test_analyze_php.mli *)

(* This makes accessible the different test_xxx functions above from 
 * the command line, e.g. '$ pfff_misc -cfg_php foo.php' will call the 
 * test_cfg_php function.
 *)
val actions: unit -> Common.cmdline_actions

(* helpers used also in unit_analyze_php.ml *)
val db_of_files_or_dirs: Common.path list -> Database_php.database

(*x: test_analyze_php.mli *)
(*e: test_analyze_php.mli *)
