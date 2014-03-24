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
 * 
 * related work:
 *  - http://jquery.cs.ubc.ca/, the original inspiration for codequery
 *  - QL by semmle
 *  - ndepend.com CQL
 * 
 * notes: pieter started to implement something similar using neo4j/cypher
 * instead of prolog for the query engine. Example of query:
 *   MATCH (n {vmname: "com/facebook/inject/AbstractProvider"})<-[:EXTENDS]-(m)
 *   RETURN m.vmname
 *   LIMIT 500
 * The main advantage is that if you have your linter already written in
 * Java, then neo4j APIs are easily accessible from the linter to get
 * access to global information. The equivalent in pfff would be to use
 * the graph_code OCaml API from your ocaml linter.
 * 
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

let lang = ref "php"

(* todo: swipl (SWI-Prolog) is not in PATH by default on our machines *)
let swipl_fb = "/home/pad/packages/Linux/bin/swipl"
let swipl =
  if Sys.file_exists swipl_fb
  then swipl_fb
  else "swipl"

let predicates_file = 
  Filename.concat Config_pfff.path "h_program-lang/database_code.pl"

(* todo: should remove that at some point and be able to do everything in RAM *)
let metapath = ref "/tmp/pfff_db"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Language specific, building the prolog db *)
(*****************************************************************************)
let build_prolog_db lang root =
  let root = Common.realpath root +> Common2.chop_dirsymbol in
  let files = Find_source.files_of_root ~lang:"lang" root in
  match lang with
  | "php" ->
      (* 
       * todo: 
       * - do things in parallel, pack things.
       * => should significantly reduce the time to produce the
       * prolog facts. It currently takes 41min on www and I hope
       * we can reduce that to a few minutes.
       *)
       (* so many errors that is better to hide them for now *)
       Flag_analyze_php.show_errors := false;

       let facts_pl_file = "facts.pl" in
       let prolog_compiled_db = "prolog_compiled_db" in

       let file = Filename.concat !metapath facts_pl_file in
       pr2 (spf "generating prolog facts in %s" file);
       let facts =
         Database_prolog_php.build ~show_progress:!verbose root files in
       Common.command2 (spf "mkdir -p %s" !metapath);
       Common.with_open_outfile file (fun (pr_no_nl, _chan) ->
         let pr s = pr_no_nl (s ^ "\n") in
         facts +> List.iter (fun fact ->
           pr (Graph_code_prolog.string_of_fact fact);
         )
       );

       pr2 (spf "compiling prolog facts with swipl in %s/%s" 
               !metapath prolog_compiled_db);
       Common.command2 (spf "%s -c %s/%s %s" 
                           swipl !metapath facts_pl_file predicates_file);
       Common.command2 (spf "mv a.out %s/%s" !metapath prolog_compiled_db);

       pr2 "";
       pr2 (spf "Your compiled prolog DB is ready. Run %s/%s"
               !metapath prolog_compiled_db);

  | "cmt" | "bytecode" | "clang2" ->
      
      let g = 
        match lang with

#if FEATURE_CMT
        | "cmt" -> 
          let ml_files = Find_source.files_of_root ~lang:"ml" root in
          let cmt_files = files in
          Graph_code_cmt.build ~verbose:!verbose ~root ~cmt_files ~ml_files
#endif

#if FEATURE_BYTECODE
        | "bytecode" -> 
          let graph_code_java =  
(*           Some (Graph_code_java.build ~verbose:!verbose ~only_defs:true
                    root skip_list) 
*)
            None
          in
          Graph_code_bytecode.build ~verbose:!verbose ~graph_code_java 
            root files 
#endif

        | "clang2" -> Graph_code_clang.build ~verbose:!verbose root files
        | _ -> raise Impossible
      in
      let facts = Graph_code_prolog.build g in
      let facts_pl_file = Filename.concat root "facts.pl" in
      Common.with_open_outfile facts_pl_file (fun (pr_no_nl, _chan) ->
        let pr s = pr_no_nl (s ^ "\n") in
        facts +> List.iter (fun x -> pr (Graph_code_prolog.string_of_fact x))
      );
      let prolog_compiled_db = Filename.concat root "prolog_compiled_db" in
      Common.command2 (spf "%s -c %s %s" swipl facts_pl_file predicates_file);
      Common.command2 (spf "mv a.out %s" prolog_compiled_db);
      pr2 (spf "Your compiled proog DB is ready. Run %s" prolog_compiled_db);
      ()

  | _ -> failwith ("language not yet supported: " ^ lang)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action _xs =
  Logger.log Config_pfff.logger "codequery" None;
  raise Todo

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)
let test () =
  let suite = Unit_prolog_php.unittest in
  OUnit.run_test_tt suite +> ignore;
  ()

(* ---------------------------------------------------------------------- *)
let extra_actions () = [
  "-build", " <dir> source code to analyze",
  Common.mk_action_1_arg (fun dir -> build_prolog_db !lang dir);
  "-test", " run regression tests",
  Common.mk_action_0_arg test;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  extra_actions () @
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
  ] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
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
  Gc.set {(Gc.get ()) with Gc.stack_limit = 200 * 1024 * 1024};
  Flag_analyze_php.verbose_database := false;

  let usage_msg = 
    spf "Usage: %s [options] <dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
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
