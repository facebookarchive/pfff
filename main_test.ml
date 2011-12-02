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

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)

open OUnit 

let test regexp = 

  (* There is no reflection in OCaml so the unit test framework OUnit requires
   * us to explicitely build the test suites (which is not that bad).
   *)
  let tests = 
    "all" >::: [
      Unit_parsing_php.unittest;
      Unit_pretty_print_php.unittest;
      Unit_matcher_php.unittest;
      Unit_checker_php.unittest;

      Unit_foundation_php.unittest;
      Unit_prolog_php.unittest;
      Unit_static_analysis_php.unittest;
      Unit_static_analysis_simple_php.unittest;
      Unit_analyze_db_php.unittest;
      (* this one needs xdebug to work *)
      Unit_coverage_php.unittest;

      (* non PHP related tests *)
      Unit_parsing_js.unittest;
      Unit_parsing_html.unittest;
      Unit_parsing_ml.unittest;
    ]
  in
  let suite = 
    if regexp = "all"
    then tests
    else
      let paths = 
        OUnit.test_case_paths tests |> List.map OUnit.string_of_path in
      let keep = paths 
        +> Common.filter (fun path -> 
          pr2 path;
          path =~ (".*" ^ regexp)) 
      in
      Common.some (OUnit.test_filter keep tests)
  in
    
  OUnit.run_test_tt ~verbose:!verbose suite +> ignore;
  ()

let main_action x = 
  test x

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
let action1 () = 
  raise Todo


let test_json_pretty_printer file =
  let json = Json_in.load_json file in
  let s = Json_io.string_of_json json in
  pr s

let test_json_bench file =
  Common.profile_code2 "json_bench" (fun () ->
    pr2 (Common.memory_stat ());
    let _json = Json_in.load_json file in
    pr2 (Common.memory_stat ());
  )

(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
  "-json_pp", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  "-json_bench", " <file>",
  Common.mk_action_1_arg test_json_bench;
  
  "-check_overlay", " <dir_orig> <dir_overlay>",
  Common.mk_action_2_arg (fun dir_orig dir_overlay ->
    Overlay_code.check_overlay ~dir_orig ~dir_overlay;
  );
  "-gen_overlay", " <dir_orig> <dir_overlay> <output>",
  Common.mk_action_3_arg (fun dir_orig dir_overlay output ->
    Overlay_code.gen_overlay ~dir_orig ~dir_overlay ~output;
  );

  "-adapt_layers_overlay", " <overlay> <dir_layers> <dir_ayers_overlay>",
  Common.mk_action_3_arg 
    (fun overlay dir_layers_orig dir_layers_overlay ->
    Overlay_code.adapt_layers 
      ~overlay:(Overlay_code.load_overlay overlay)
      ~dir_layers_orig 
      ~dir_layers_overlay
      ;
  );
  "-adapt_database_overlay", "<overlay> <file> <output>",
  Common.mk_action_3_arg (fun overlay orig output ->
    let db = 
      Database_code.load_database orig in
    let db' = 
      Overlay_code.adapt_database db (Overlay_code.load_overlay overlay) in
    Database_code.save_database db' output
  );
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() ++
 Test_parsing_php.actions()++
 Test_analyze_php.actions()++
 Builtins_php.actions()++
  []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++

  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_other () ++

  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff (test) version: %s" Config.version);
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

  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};
  Common_extra.set_link(); 

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
    | [x] ->
        main_action x

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | _ -> 
        Common.usage usage_msg (options()); 
        failwith "too few or too many arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
    main ();
  )
