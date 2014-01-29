(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)
open Common

open OUnit 

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests runner (and a few adhoc actions) *)

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
(* Helpers *)
(*****************************************************************************)

let ast_fuzzy_of_string str =
  Common2.with_tmp_file ~str ~ext:"cpp" (fun tmpfile ->
    Parse_cpp.parse_fuzzy tmpfile +> fst
  )

let graph_of_string str =
  let tmpfile = Parse_php.tmp_php_file_from_string str in
  let (g, _stat) = Graph_code_php.build 
    ~verbose:false ~logfile:"/dev/null" "/" [tmpfile] in
  g
  

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)

let test regexp = 

  (* There is no reflection in OCaml so the unit test framework OUnit requires
   * us to explicitely build the test suites (which is not that bad).
   *)
  let tests = 
    "all" >::: [

      (* general tests *)
      Unit_program_lang.unittest;
      Unit_graph_code.unittest ~graph_of_string;
      Unit_version_control.unittest;
      Unit_matcher.sgrep_unittest ~ast_fuzzy_of_string;
      (* todo: Unit_matcher.spatch_unittest ~xxx *)

      (* PHP related tests *)
      Unit_parsing_php.unittest;
      Unit_pretty_print_php.unittest;
      Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *)
      Unit_foundation_php.unittest;
      Unit_static_analysis_php.unittest;
      Unit_typeinfer_php.unittest;
(*      Unit_analyze_db_php.unittest; *)
(*      Unit_static_analysis_simple_php.unittest;*)
      Unit_prolog_php.unittest;
      Unit_checker_php.unittest;
      (* this one needs xdebug to work *)
      Unit_coverage_php.unittest;

      (* non PHP related tests *)

      Unit_parsing_ml.unittest;
      Unit_analyze_ml.unittest;
      Unit_parsing_java.unittest;
      Unit_analyze_java.unittest;
      Unit_analyze_bytecode.unittest;
      Unit_parsing_js.unittest;
      Unit_analyze_js.unittest;
      Unit_parsing_html.unittest;
      Unit_parsing_opa.unittest;
      Unit_parsing_cpp.unittest;
      Unit_parsing_objc.unittest;
    ]
  in
  let suite = 
    if regexp = "all"
    then tests
    else
      let paths = 
        OUnit.test_case_paths tests +> List.map OUnit.string_of_path in
      let keep = paths 
        +> List.filter (fun path -> 
          pr2 path;
          path =~ (".*" ^ regexp)) 
      in
      Common2.some (OUnit.test_filter keep tests)
  in
    
  let results = OUnit.run_test_tt ~verbose:!verbose suite in
  let has_an_error = 
    results +> List.exists (function
    | OUnit.RSuccess _ | OUnit.RSkip _ | OUnit.RTodo _ -> false
    | OUnit.RFailure _ | OUnit.RError _ -> true
    )
  in
  raise (Common.UnixExit (if has_an_error then 1 else 0))

let main_action x = 
  test x

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
type x = Foo of int

let action1 () = 
  let x = Foo 1 in
  pr2_gen x;
  let x = 1 in
  pr2_gen x;
  ()



let test_json_pretty_printer file =
  let json = Json_in.load_json file in
  let s = Json_io.string_of_json json in
  pr s

let test_json_bench file =
  Common.profile_code "json_bench" (fun () ->
    pr2 (Common2.memory_stat ());
    let _json = Json_in.load_json file in
    pr2 (Common2.memory_stat ());
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
  "-action1", "", Common.mk_action_0_arg action1;

  "-action2", "<files>", Common.mk_action_n_arg (fun xs ->
    xs +> List.iter (fun file ->
      let (_d,b,_e) = Common2.dbe_of_filename file in
      let (d,b,e) = "/home/pad/plan9/sys/src/include", b, "h.clang2" in
      let file = Common2.filename_of_dbe (d,b,e) in
      if Sys.file_exists file
      then ()
      else (* pr2 (spf "file %s not there" file); *)
        print_string (spf "%s " b)
    );
    print_string "\n";
  );

]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() ++
 Test_parsing_php.actions()++
 Test_analyze_php.actions()++
 Test_analyze_js.actions()++
 Test_analyze_ml.actions()++
 Test_analyze_clang.actions()++
 Test_program_lang.actions()++
 Builtins_php.actions()++
  []

let options () = [
  "-verbose", Arg.Set verbose, 
  " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common2.cmdline_flags_devel () ++
  Common2.cmdline_flags_other () ++
  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff (test) version: %s" Config_pfff.version);
      exit 0;
    ), 
    "  guess what";
    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () -> 
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ), 
    "   guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};

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
