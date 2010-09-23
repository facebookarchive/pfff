(*s: test_analyze_php.mli *)

(* Returns the testsuite for analyze_php/. To be concatenated by 
 * the caller (e.g. in pfff/main_misc.ml ) with other testsuites and 
 * run via OUnit.run_test_tt 
 *)
val unittest: OUnit.test

(* This makes accessible the different test_xxx functions above from 
 * the command line, e.g. '$ pfff_misc -cfg_php foo.php' will call the 
 * test_cfg_php function.
 *)
val actions: unit -> Common.cmdline_actions
(*x: test_analyze_php.mli *)
(*e: test_analyze_php.mli *)
