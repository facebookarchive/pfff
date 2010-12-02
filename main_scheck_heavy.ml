
(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

open Common

open Ast_php

module Ast = Ast_php
module V = Visitor_php

module S = Scope_code

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* A lint-like checker for PHP using (expensive) global analysis.
 * See also main_scheck.ml
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

(* In strict mode, we are more aggressive regarding scope like in
 * JsLint.
 *
 * is also in Error_php.ml
 *)
let strict_scope = ref false 

let rank = ref true

let layer_file = ref (None: filename option)

let metapath = ref "/tmp/pfff_db"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_dbg s =
  if !verbose then Common.pr2 s

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(* mostly a copy paste of main_scheck.ml but now use metapath 
 * and calls more check_xxx functions.
 *)
let main_action xs =

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
  let errors = ref [] in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;
  files +> List.iter (fun file ->
    try 
      pr2_dbg (spf "processing: %s" file);
      raise Todo;
      let find_entity = None in
      
      Check_all_php.check_file ~find_entity file;
    with exn ->
      Common.push2 (spf "PB with %s, exn = %s" file 
                              (Common.string_of_exn exn)) errors;
  );

  let errs = !Error_php._errors +> List.rev in
  let errs = 
    if !rank 
    then Error_php.rank_errors errs +> Common.take_safe 20 
    else errs 
  in

  errs +> List.iter (fun err -> pr (Error_php.string_of_error err));
  Error_php.show_10_most_recurring_unused_variable_names ();
  pr2 (spf "total errors = %d" (List.length !Error_php._errors));

  pr2 "";
  !errors +> List.iter pr2;
  pr2 "";

  !layer_file +> Common.do_option (fun file ->
    (*  a layer needs readable paths, hence the root *)
    let root = Common.common_prefix_of_files_or_dirs xs in

    Layer_checker_php.gen_layer ~root ~output:file !Error_php._errors

  );
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let scheck_extra_actions () = [
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
 scheck_extra_actions()++
 []

let options () =
  [
    "-metapath", Arg.Set_string metapath, 
    "<dir> (default=" ^ !metapath ^ ")";
    "-strict", Arg.Set strict_scope,
    " emulate block scope instead of function scope";
     "-no_scrict_scope", Arg.Clear strict_scope, 
     " use function scope (default)";
    "-no_rank", Arg.Clear rank,
    " ";
    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in pfff layer file";

    "-verbose", Arg.Set verbose,
    " ";
  ] ++
  Flag_analyze_php.cmdline_flags_verbose () ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
  "-version",   Arg.Unit (fun () ->
    pr2 (spf "scheck version: %s" Config.version);
    exit 0;
  ),
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () ->
    pr2 "version: $Date: 2010/04/25 00:44:57 $";
    raise (Common.UnixExit 0)
    ),
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  let usage_msg =
    "Usage: " ^ Common.basename Sys.argv.(0) ^
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
