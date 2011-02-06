(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)

open Common




(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  raise Todo 

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
let action1 () = 
  raise Todo


let test_json_pretty_printer file =
  let json = Json_in.load_json file in
  let s = Json_io.string_of_json json in
  pr s


(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
  "-json_pp", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  
  "-layer_stat", " <file>",
  Common.mk_action_1_arg Test_program_lang.layer_stat;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() ++
  Test_parsing_ml.actions()++
  Test_parsing_php.actions()++
  Test_parsing_js.actions()++
  Test_parsing_cpp.actions()++
  Test_parsing_nw.actions()++
  Test_parsing_lisp.actions()++
  Test_parsing_hs.actions()++
  Test_parsing_python.actions()++
  Test_parsing_csharp.actions()++
  Test_parsing_java.actions()++
  Test_parsing_erlang.actions()++
  Test_mini_php.actions()++

  Test_analyze_cpp.actions () ++
  []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  Flag_parsing_php.cmdline_flags_pp () ++
  Flag_parsing_php.cmdline_flags_verbose () ++
  Flag_parsing_php.cmdline_flags_debugging () ++

  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_other () ++

  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff (console) version: %s" Config.version);
      exit 0;
    ), 
    "  guess what";

    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () -> 
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ), 
    "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  (* Common_extra.set_link(); 
     let argv = Features.Distribution.mpi_adjust_argv Sys.argv in
  *)

  let usage_msg = 
    "Usage: " ^ basename Sys.argv.(0) ^ 
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
