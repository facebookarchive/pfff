(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)

open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* A module/package dependency visualizer generating data for gephi.
 * See http://gephi.org.
 * 
 * todo? have a backend for graphviz?
 *
 * usage: 
 *  $ pm_depend [-lang X] [-with-extern] [-depth n] -o pfff.gexf /path/to/dir
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

let verbose = ref false

let with_extern = ref false
let package_depth = ref 0

let lang = ref "ml"

let output_file = ref "/tmp/pm.gexf"

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

let rec dependencies_of_files_or_dirs lang xs = 
  let verbose = !verbose in
  match lang, xs with
  | "ml", [dir] ->
      Graph_modules_packages_ml.dependencies
        ~verbose
        ~with_extern:!with_extern
        ~package_depth:!package_depth
        dir
  | _ -> failwith ("language not supported: " ^ lang)
      
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  let g = dependencies_of_files_or_dirs !lang xs in
  pr2 (spf "Writing data in %s" !output_file);
  g +> Graph_gephi.graph_to_gefx 
    ~str_of_node:(fun s -> s)
    ~tree:None
    ~weight_edges:None
    ~output:!output_file;
  ()

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  Test_parsing_ml.actions()++
  []

let options () = 
  [
    "-with_extern", Arg.Set with_extern,
    " includes external references";
    "-package_mode", Arg.Set_int package_depth,
    " <n> project at depth n";
    "-verbose", Arg.Set verbose, 
    " ";
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);
    "-o", Arg.Set_string output_file, 
    (spf " <file> default = %s" !output_file);
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_other () ++
  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pm_depend version: %s" Config.version);
      exit 0;
    ), 
    "  guess what";
    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () -> 
      pr2 "version: $Date: 2011/09/01 00:44:57 $";
      raise (Common.UnixExit 0)
    ), 
    "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  (* Common_extra.set_link(); *)
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
