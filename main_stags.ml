(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)

open Common

module Tags = Tags_file

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* A more precise etags/ctags. 
 *
 * etags/ctags are essentially working at a lexical level
 * and so have lots of false positive for their definitions. For instance
 * on PHP a line containing $idx will be considered as a candidate when
 * searching for idx. By using a real parser tagging a file becomes
 * trivial and correct (but slower I have to admit).
 * 
 * Note that for OCaml the situation is reversed. The tags
 * generator 'otags' can work only at a AST level, which requires to
 * correctly parse the file. Nevertheless many files using camlp4 are
 * causing otags to fatal. One option is to help otags by passing it
 * the correct -pp flags. Another option is to at least default to
 * a lexical-level tag generator which is what I do here.
 * 
 * usage: 
 *  $ stags -lang web -o TAGS *
 *)

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

(* obsolete ? *)
let heavy_tagging = ref false

let lang = ref "php"

let output_file = ref "TAGS"

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

let rec defs_of_files_or_dirs lang xs = 
  let verbose = !verbose in
  let _heavy_tagging = !heavy_tagging in
  match lang with
  | "php" ->
      Tags_php.php_defs_of_files_or_dirs ~verbose (*~heavy_tagging*) xs 
  | "js" ->
      Tags_js.tags_of_files_or_dirs ~verbose xs
  | "ml" ->
      Tags_ml.defs_of_files_or_dirs ~verbose xs

  | "web" ->
      let tag1 = defs_of_files_or_dirs "php" xs in
      let tag2 = defs_of_files_or_dirs "js" xs in
      tag1 ++ tag2

  | _ -> failwith ("language not supported: " ^ lang)
      
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =

  (* This can augment the size of the tags file
     let xs = List.map relative_to_absolute xs in 
  *)
  let tags_file = 
    (*
     * Common.relative_to_absolute "TAGS" in
     * let res = Common.y_or_no (spf "writing data in %s" tag_file) in
     * if not res 
     * then failwith "ok I stop";
     *)
    !output_file
  in
  let files_and_defs = defs_of_files_or_dirs !lang xs in
  pr2 (spf "Writing data in %s" tags_file);
  Tags.generate_TAGS_file ~tags_file files_and_defs;
  ()

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
    "-heavy_tagging", Arg.Set heavy_tagging, 
    " generates some extra tags with semantic prefix: F_, C_, M_";

    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);

    "-o", Arg.Set_string output_file, 
    " <file> default = TAGS";

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
