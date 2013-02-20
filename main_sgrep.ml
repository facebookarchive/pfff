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

open Ast_php

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* 
 * A syntactical grep for PHP. https://github.com/facebook/pfff/wiki/Sgrep
 * 
 * opti: git grep xxx | xargs sgrep_php
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false


let pattern_file = ref ""
let pattern_string = ref ""

let case_sensitive = ref false
let match_format = ref Lib_parsing_php.Normal

let mvars = ref ([]: Metavars_php.mvar list)

let layer_file = ref (None: filename option)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for -gen_layer *)
let _matching_tokens = ref []

(* TODO? could do slicing of function relative to the pattern, so 
 * would see where the parameters come from :)
 *)

let print_match mvars mvar_binding tokens_matched_code = 
  (match mvars with
  | [] ->
      Lib_parsing_php.print_match ~format:!match_format tokens_matched_code
  | xs ->
      (* similar to the code of Lib_parsing_php.print_match, maybe could
       * factorize code a bit.
       * This assumes there is no FakeTok in tokens_matched_code.
       * Currently the only fake tokens generated in parser_php.mly are
       * for abstract methods and sgrep/spatch do not have metavariables
       * to match such construct so we should be safe.
       *)
      let (mini, maxi) = 
        Lib_parsing_php.min_max_ii_by_pos tokens_matched_code in
      let (file, line) = 
        Ast.file_of_info mini, Ast.line_of_info mini in

      let strings_metavars =
        xs +> List.map (fun x ->
          match Common2.assoc_option x mvar_binding with
          | Some any ->
              Lib_parsing_php.ii_of_any any
              +> List.map Ast.str_of_info 
              +> Lib_parsing_php.join_with_space_if_needed
          | None ->
              failwith (spf "the metavariable '%s' was not binded" x)
          )
      in
      pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
  );
  tokens_matched_code +> List.iter (fun x -> Common.push2 x _matching_tokens)

let print_simple_match tokens_matched_code =
  print_match [] [] tokens_matched_code


(* a layer need readable path, hence the ~root argument *)
let gen_layer ~root ~query file =
  pr2 ("generating layer in " ^ file);

  let root = Common2.relative_to_absolute root in

  let toks = !_matching_tokens in
  let kinds = ["m" (* match *), "red"] in
  
  (* todo: could now use Layer_code.simple_layer_of_parse_infos *)
  let files_and_lines = toks +> List.map (fun tok ->
    let file = Ast.file_of_info tok in
    let line = Ast.line_of_info tok in
    let file' = Common2.relative_to_absolute file in 
    Common.filename_without_leading_path root file', line
  )
  in
  let group = Common.group_assoc_bykey_eff files_and_lines in
  let layer = { Layer_code.
    title = "Sgrep";
    description = "output of sgrep";
    kinds = kinds;
    files = group +> List.map (fun (file, lines) ->
      let lines = Common2.uniq lines in
      (file, { Layer_code.
               micro_level = (lines +> List.map (fun l -> l, "m"));
               macro_level =  if null lines then [] else ["m", 1.];
      })
    );
  }
  in
  Layer_code.save_layer layer file;
  ()
  
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action xs =
  let pattern, query_string = 
    match !pattern_file, !pattern_string with
    | "", "" -> 
        failwith "I need a pattern; use -f or -e";
    | file, _ when file <> "" ->
        let s = Common.read_file file in
        Sgrep_php.parse s, s
    | _, s when s <> ""->
        Sgrep_php.parse s, s
    | _ -> raise Impossible
  in
  Logger.log Config_pfff.logger "sgrep" (Some query_string);

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    Sgrep_php.sgrep 
      ~case_sensitive:!case_sensitive 
      ~hook:(fun env matched_tokens -> 
        print_match !mvars env matched_tokens
      )
      pattern 
      file 
  );

  !layer_file +> Common.do_option (fun file ->
    let root = Common2.common_prefix_of_files_or_dirs xs in
    gen_layer ~root ~query:query_string  file
  );
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)
let dump_sgrep_pattern file =
  let any = Parse_php.parse_any file in
  let s = Export_ast_php.ml_pattern_string_of_any any in
  pr s


(*---------------------------------------------------------------------------*)
(* Regression testing *)
(*---------------------------------------------------------------------------*)
open OUnit
let test () =
  let suite = "sgrep" >::: Unit_matcher_php.sgrep_unittest in
  OUnit.run_test_tt suite +> ignore;
  ()

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let sgrep_extra_actions () = [
  "-dump_pattern", " <file> (internal)",
  Common.mk_action_1_arg dump_sgrep_pattern;
  "-test", " run regression tests",
  Common.mk_action_0_arg test;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 sgrep_extra_actions()++
 []

let options () = 
  [
    "-e", Arg.Set_string pattern_string, 
    " <pattern> expression pattern";
    "-f", Arg.Set_string pattern_file, 
    " <file> obtain pattern from file";

    "-case_sensitive", Arg.Set case_sensitive, 
    " match code in a case sensitive manner";

    "-emacs", Arg.Unit (fun () -> match_format := Lib_parsing_php.Emacs ),
    " print matches on the same line than the match position";
    "-oneline", Arg.Unit (fun () -> match_format := Lib_parsing_php.OneLine),
    " print matches on one line, in normalized form";

    "-pvar", Arg.String (fun s -> mvars := Common.split "," s),
    " <metavars> print the metavariables, not the matched code";

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in a pfff layer file";

    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      Flag_matcher_php.verbose := true;
    ),
    " ";
  ] ++
  (* old: Flag_parsing_php.cmdline_flags_pp () ++ *)
  Common.options_of_actions action (all_actions()) ++
  Common2.cmdline_flags_devel () ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "sgrep_php version: %s" Config_pfff.version);
    exit 0;
  ), 
    "  guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    spf "Usage: %s [options] <pattern> <file or dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Sgrep"
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
