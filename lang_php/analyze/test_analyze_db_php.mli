(*s: test_analyze_db_php.mli *)
(* This makes accessible the different test_xxx functions above from 
 * the command line, e.g. '$ pfff_misc -cfg_php foo.php' will call the 
 * test_cfg_php function.
 *)
val actions: unit -> Common.cmdline_actions
(*e: test_analyze_db_php.mli *)
