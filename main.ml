(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*  
 * A "driver" for the different parsers in pfff.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

let verbose = ref false

let lang = ref "c"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action _xs = 
  raise Todo 

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
let test_json_pretty_printer file =
  let json = Json_in.load_json file in
  let s = Json_io.string_of_json json in
  pr s



(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
  "-dump_json", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  "-json_pp", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  "-layer_stat", " <file>",
  Common.mk_action_1_arg Test_program_lang.layer_stat;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() @
  Test_parsing_ml.actions()@

  Test_parsing_php.actions()@
  Test_parsing_js.actions()@

  Test_parsing_c.actions()@
  Test_parsing_cpp.actions()@
  Test_parsing_clang.actions()@
  Test_mini.actions()@
(*
  Test_parsing_bytecode.actions()++
*)
  Test_parsing_java.actions()@

  Test_parsing_nw.actions()@

  Test_parsing_lisp.actions()@
  Test_parsing_hs.actions()@

  Test_parsing_python.actions()@
  Test_parsing_csharp.actions()@

  Test_parsing_rust.actions()@
  Test_parsing_erlang.actions()@

  Test_parsing_text.actions()@
  Test_parsing_html.actions()@
  Test_parsing_css.actions()@
  Test_parsing_web.actions()@

  Test_parsing_opa.actions()@
  Test_parsing_sql.actions()@

(*
  Test_analyze_cpp.actions () ++
  Test_analyze_php.actions () ++
  Test_analyze_ml.actions () ++
  Test_analyze_clang.actions () ++
  Test_analyze_c.actions() ++
*)
  []


let options () = [
  "-verbose", Arg.Set verbose, 
  " ";
  "-lang", Arg.Set_string lang, 
  (spf " <str> choose language (default = %s)" !lang);
  ] @
  Flag_parsing_php.cmdline_flags_verbose () @
  Flag_parsing_cpp.cmdline_flags_verbose () @

  Flag_parsing_php.cmdline_flags_debugging () @
  Flag_parsing_cpp.cmdline_flags_debugging () @

  Flag_parsing_php.cmdline_flags_pp () @
  Flag_parsing_cpp.cmdline_flags_macrofile () @

  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  Common2.cmdline_flags_other () @
  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff version: %s" Config_pfff.version);
      exit 0;
    ), "  guess what";
  ]


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  (* Common_extra.set_link(); 
     let argv = Features.Distribution.mpi_adjust_argv Sys.argv in
  *)

  let usage_msg = 
    "Usage: " ^ Common2.basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 
    
    (match args with
    
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)
          
    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
  )
