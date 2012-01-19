

(* Print the set of tokens in a .opa file *)
val test_tokens_opa : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from 
 * the command line, e.g. '$ pfff -parse_opa foo.opa will call the 
 * test_parse_opa function.
 *)
val actions : unit -> Common.cmdline_actions
