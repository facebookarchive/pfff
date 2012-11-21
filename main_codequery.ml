(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * An interactive tool a la SQL to query information about the structure
 * of a codebase (the inheritance tree, the call graph, the data graph),
 * for instance "What are all the children of class Foo?". 
 * The data is the code. The query language is
 * Prolog (http://en.wikipedia.org/wiki/Prolog), a logic-based
 * programming language used mainly in AI but also popular in database
 * (http://en.wikipedia.org/wiki/Datalog).
 * 
 * See h_program-lang/database_code.pl for more information
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

let lang = ref "php"

(* todo: should remove that at some point and be able to do everything in RAM *)
let metapath = ref "/tmp/pfff_db"
(* todo: swipl (SWI-Prolog) is not in PATH by default on our machines *)
let swipl = "/home/pad/packages/Linux/bin/swipl"
let predicates_file = "/home/engshare/pfff/database_code.pl"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Language specific, building the prolog db *)
(*****************************************************************************)
let build_prolog_db lang root =
  match lang with
  | "php" ->
      (* 
       * todo: 
       * - simplify and do everything in memory; do not use berkeley DB.
       * - do not use database_php.ml, do simple visit on ast_php_simple.ml
       * - do things in parallel, pack things.
       * => should significantly reduce the time to produce the
       * prolog facts. It currently takes 41min on www and I hope
       * we can reduce that to a few minutes.
       * 
       * note: copy paste of www_db_build, yet another one ...
       *)
       let dir = Common.realpath root +> Common.chop_dirsymbol in
       (* so many errors that is better to hide them for now *)
       Flag_analyze_php.show_errors := false;

       let phase = Database_php_build.max_phase in
       (* you can also look at /tmp/pfff_db/log.log in case of problems *)
       let db =
         Database_php_build.create_db
           ~db_support:(Database_php.Disk !metapath)
           ~phase
           ~annotate_variables_program:None
           ~verbose_stats:!verbose
           (Database_php.prj_of_dir dir)
       in
       Database_php.close_db db;

       let facts_pl_file = "facts.pl" in
       let prolog_compiled_db = "prolog_compiled_db" in


       let file = Filename.concat !metapath facts_pl_file in
       pr2 (spf "generating prolog facts in %s" file);
       Database_php.with_db ~metapath:!metapath (fun db ->
         Database_prolog_php.gen_prolog_db ~show_progress:!verbose db file;
       );
       pr2 (spf "compiling prolog facts with swipl in %s/%s" 
               !metapath prolog_compiled_db);
       Common.command2 (spf "%s -c %s/%s %s" 
                           swipl !metapath facts_pl_file predicates_file);
       Common.command2 (spf "mv a.out %s/%s" !metapath prolog_compiled_db);

       pr2 "";
       pr2 (spf "Your compiled prolog DB is ready. Run %s/%s"
               !metapath prolog_compiled_db);

  | _ -> failwith ("language not yet supported: " ^ lang)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  Logger.log Config_pfff.logger "codequery" None;
  raise Todo

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
let extra_actions () = [
  "-build", " <dir> source code to analyze",
  Common.mk_action_1_arg (fun dir -> build_prolog_db !lang dir);
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  extra_actions () ++
  []

let options () = [
  "-lang", Arg.Set_string lang, 
  (spf " <str> choose language (default = %s)" !lang);

  "-metapath", Arg.Set_string metapath, 
  (spf " <dir> where to put the data (default = %s)" !metapath);

  "-verbose", Arg.Unit (fun () ->
    verbose := true;
    Flag_analyze_php.verbose_database := true;
  ), " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "CodeQuery version: %s" Config_pfff.version);
      exit 0;
    ), 
    "  guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  Database_php_storage.set_link();
  Common_extra.set_link();
  Gc.set {(Gc.get ()) with Gc.stack_limit = 200 * 1024 * 1024};
  Flag_analyze_php.verbose_database := false;

  let usage_msg = 
    spf "Usage: %s [options] <dir> \nDoc: %s\nOptions:"
      (Common.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Codequery"
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
