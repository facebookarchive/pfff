open Common

open Ast_php

module Ast = Ast_php
module Db = Database_php
module V = Visitor_php

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

let metapath = ref "/tmp/pfff_db"

(* for build_db *)
let phase = ref Database_php_build.max_phase

let index_method = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action, build the database *)
(*****************************************************************************)

let main_action xs = 
  match xs with
  | [dir] -> 

      let dir = Common.realpath dir |> Common.chop_dirsymbol in
      let prj = Database_php.Project (dir, None) in
      let prj = Database_php.normalize_project prj in 

      let db = 
        Database_php_build.create_db
          ~db_support:(Database_php.Disk !metapath)
          ~phase:!phase
          prj 
      in
      if !index_method then Database_php_build.index_db_method db;

      Database_php.close_db db;
      ()
  | x::y::ys ->
      raise Todo

  | [] -> raise Impossible

let layers = [
  "layer_deadcode.json", (fun dir db layerfile ->
    Layer_deadcode_php.gen_layer ~db ~output:layerfile;
  );
  "layer_bugs.json", (fun dir db layerfile ->
    
    (* mostly copy paste of main_scheck_heavy.ml *)
    let files = Lib_parsing_php.find_php_files_of_dir_or_files [dir] in
    let errors = ref [] in
    let find_entity = Some (Database_php_build.build_entity_finder db) in

    files +> Common.index_list_and_total +> List.iter (fun (file, i, total) ->
      try 
        pr2 (spf "processing: %s (%d/%d)" file i total);
        Check_all_php.check_file ~find_entity file;
      with 
      | (Timeout | UnixExit _) as exn -> raise exn
      | exn ->
          Common.push2 (spf "PB with %s, exn = %s" file 
                           (Common.string_of_exn exn)) errors;
    );
    !errors +> List.iter pr2;
    Layer_checker_php.gen_layer ~root:dir ~output:layerfile !Error_php._errors
  );
]

let gen_layers metapath =
  Db.with_db ~metapath (fun db ->
    let dir = Db.path_of_project_in_database db in
    pr2 (spf "generating layers for %s in %s" dir metapath);

    layers +> List.iter (fun (layerfile, f) ->
      f dir db (Filename.concat metapath layerfile);
    );
  )

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

let pfff_extra_actions () = [
  "-gen_layers", " <metapath>",
  Common.mk_action_1_arg gen_layers;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() ++
  Database_php_build.actions() ++
  (* Layer_checker is in main_scheck.ml *)
  Layer_deadcode_php.actions () ++
  Layer_coverage_php.actions () ++
  Layer_xhprof.actions () ++
  []

let options () = 
  [
    "-metapath", Arg.Set_string metapath, 
    "<dir> (default=" ^ !metapath ^ ")";
    "-phase", Arg.Set_int phase,
    " <phase number>";
    "-index_method", Arg.Set index_method,
    " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Flag_parsing_php.cmdline_flags_verbose () ++
  Flag_parsing_php.cmdline_flags_debugging () ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff db (console) version: %s" Config.version);
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

  Common_extra.set_link();
  (* let argv = Features.Distribution.mpi_adjust_argv Sys.argv in *)
  Database_php_storage.set_link();

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
