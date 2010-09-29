(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)

open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* Something lighter than a full berkeley database but more powerful than
 * just a TAGS file. Such a light_database can be leverage by
 * the code/treemap visualizer to convey visually semantic
 * information that can help navigate a big codebase with huge API.
 * 
 * Try to leverage multiple code artifacts:
 *  - the source code
 *  - SEMI the test coverage (the static part)
 *  - TODO some static analysis (deadcode, SEMI bad smell, type inference, etc)
 *  - TODO some dynamic analysis (tainting, test coverage)
 *  - TODO version history
 *  - documentation such as API reference or PLEAC cookbooks
 * 
 * See also vision.txt
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

let verbose = ref false

let db_file = ref "/tmp/light_db.db"
let pleac_dir = ref "/tmp/pleac"

(* action mode *)
let action = ref ""

let lang = ref "web"

let with_php_db = ref ""

let readable_db = ref false
let marshall_db = ref true

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

let rec light_db_of_files_or_dirs lang xs = 
  let verbose = !verbose in

  let db = 
    match lang with
    | "ml" ->
        Database_light_ml.compute_database ~verbose xs

    | "php" ->
        (match xs with
        | [_x] ->
            
            (try 
              Database_php.check_is_database_dir !with_php_db
            with _ ->
              failwith "for PHP we expect a database; use -with_php_db"
            );
            Database_php.with_db ~metapath:!with_php_db (fun db ->
              Database_light_php.database_code_from_php_database 
                ~verbose db
            )
        | _ -> 
            failwith "for PHP we expect one dir"
        )
    | "js" ->
        Database_light_js.compute_database ~verbose xs

    | "cpp" ->
        Database_light_cpp.compute_database ~verbose xs


    | "web" ->
        let db1 = light_db_of_files_or_dirs "js"  xs in
        let db2 = light_db_of_files_or_dirs "php" xs in
        Database_code.merge_databases db1 db2

    | _ -> failwith ("language not supported: " ^ lang)
  in
  db
      
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =

  let xs = xs +> List.map Common.relative_to_absolute in
  let db = light_db_of_files_or_dirs !lang xs in
  
  let file = Common.relative_to_absolute !db_file in
  let res = Common.y_or_no (spf "writing data in %s" file) in
  if not res 
  then failwith "ok I stop";

  if !marshall_db
  then Common.write_value db file
  else Database_code.save_database ~readable_db:!readable_db db file
  ;
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Pleac *)
(*---------------------------------------------------------------------------*)

(* Why put pleac code here ? Because pleac.ml helps generate code from
 * pleac.data files which transforms all this doc into regular code
 * that can be indexed as any other code and shown via the visualizer.
 * You can then benefit for free from the navigation/visualization of 
 * pfff_visual and in turn improves the pfff_visual experience by 
 * being the source for a good "goto-example-or-test" repository of code.
 * We just leverage another "code-artifact".
 * 
 * ex: ./pfff_db_light -lang ml -gen_pleac 
 *       ~/software-src/pleac/
 * ex: ./pfff_db_light -lang php -output_dir ~/www/flib/pleac/ 
 *       -gen_pleac ~/software-src/pleac/
 *)
let gen_pleac pleac_src =
  let skeleton_file = Filename.concat pleac_src "skeleton.sgml" in

  (* The section names are in the skeleton file *)
  let skeleton =
    Pleac.parse_skeleton_file skeleton_file in

  let pleac_data_file, ext_file, 
    hook_start_section2, hook_line_body, hook_end_section2 =
    match !lang with
    | "ml" ->
        "ocaml", "ml", 
        (* TODO if at some point we use a real parser for ML, 
         * this will not work ...
         *)
        (fun s -> spf "let pleac_%s () = " s),
        
        (* ugly: for now my ocaml tagger does not index functions
         * when they are not at the toplevel so add this extra
         * space
         *)
        (fun s -> " " ^ s),
        (fun s -> "")

    | "php" ->
        "php", "php", 
        (* TODO if at some point we use a real parser for ML, 
         * this will not work ...
         *)
        (fun s -> spf "<?php\nfunction pleac_%s() {" s),
        (fun s -> s),
        (fun s -> "}\n?>\n")

    | _ -> failwith (spf "language %s is not yet supported" !lang)
  in
  let pleac_data_file = 
    Filename.concat pleac_src (spf "pleac_%s.data" pleac_data_file)
  in

  let code_sections = 
    Pleac.parse_data_file pleac_data_file in
  let comment_style = 
    Pleac.detect_comment_style pleac_data_file in

  Pleac.gen_source_files skeleton 
    code_sections comment_style 
    ~output_dir:!pleac_dir
    ~gen_mode:Pleac.OneDirPerSection
    ~ext_file
    ~hook_start_section2
    ~hook_line_body
    ~hook_end_section2
  ;
  ()

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
  "-gen_pleac", " <pleac_src>; works with -lang",
  Common.mk_action_1_arg (gen_pleac);
]


(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  extra_actions() ++
  Test_program_lang.actions () ++
  []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);

    "-o", Arg.Set_string db_file, 
    (spf " <file> output file (default = %s)" !db_file);

    "-with_php_db", Arg.Set_string with_php_db, 
    (" <metapath>");

    "-output_dir", Arg.Set_string pleac_dir, 
    (spf " <dir> output file (default = %s)" !pleac_dir);

    "-readable_db", Arg.Set readable_db, 
    (" ");

  ] ++

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
