(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

module PI = Parse_info
module S = Scope_code

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* 
 * A syntactical grep. https://github.com/facebook/pfff/wiki/Sgrep
 * Right now there is support for PHP, C/C++/ObjectiveC, OCaml, Java, and 
 * Javascript.
 * 
 * opti: git grep foo | xargs sgrep -e 'foo(...)'
 * 
 * related: 
 *  - SSR http://www.jetbrains.com/idea/documentation/ssr.html
 *  - ack http://beyondgrep.com/
 *  - cgrep http://awgn.github.io/cgrep/
 *  - hound https://codeascraft.com/2015/01/27/announcing-hound-a-lightning-fast-code-search-tool/
 * 
 * See also codequery for more structural queries.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let use_multiple_patterns = ref false
let verbose = ref false

let pattern_file = ref ""
let pattern_string = ref ""

(* todo: infer from basename argv(0) ? *)
let lang = ref "php"

let case_sensitive = ref false
let match_format = ref Matching_report.Normal

let mvars = ref ([]: Metavars_fuzzy.mvar list)

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

let print_match mvars mvar_binding ii_of_any tokens_matched_code = 
  (match mvars with
  | [] ->
      Matching_report.print_match ~format:!match_format tokens_matched_code
  | xs ->
      (* similar to the code of Lib_matcher.print_match, maybe could
       * factorize code a bit.
       * This assumes there is no FakeTok in tokens_matched_code.
       * Currently the only fake tokens generated in parser_php.mly are
       * for abstract methods and sgrep/spatch do not have metavariables
       * to match such construct so we should be safe.
       *)
      let (mini, _maxi) = 
        PI.min_max_ii_by_pos tokens_matched_code in
      let (file, line) = 
        PI.file_of_info mini, PI.line_of_info mini in

      let strings_metavars =
        xs +> List.map (fun x ->
          match Common2.assoc_opt x mvar_binding with
          | Some any ->
              ii_of_any any
              +> List.map PI.str_of_info 
              +> Matching_report.join_with_space_if_needed
          | None ->
              failwith (spf "the metavariable '%s' was not binded" x)
          )
      in
      pr (spf "%s:%d: %s" file line (Common.join ":" strings_metavars));
  );
  tokens_matched_code +> List.iter (fun x -> Common.push x _matching_tokens)

let print_simple_match tokens_matched_code =
  print_match [] [] tokens_matched_code


(* a layer need readable path, hence the ~root argument *)
let gen_layer ~root ~query file =
  ignore(query);
  pr2 ("generating layer in " ^ file);

  let root = Common2.relative_to_absolute root in

  let toks = !_matching_tokens in
  let kinds = ["m" (* match *), "red"] in
  
  (* todo: could now use Layer_code.simple_layer_of_parse_infos *)
  let files_and_lines = toks +> List.map (fun tok ->
    let file = PI.file_of_info tok in
    let line = PI.line_of_info tok in
    let file' = Common2.relative_to_absolute file in 
    Common.readable root file', line
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
  

let ast_fuzzy_of_string str =
  Common2.with_tmp_file ~str ~ext:"cpp" (fun tmpfile ->
    Parse_cpp.parse_fuzzy tmpfile +> fst
  )

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

type ast_t =
  | Fuzzy of Ast_fuzzy.tree list
  | Php of Ast_php.program

let create_ast file =
  match !lang with
  | "php" ->
    Php
    (try
      (Parse_php.parse_program file)
    with Parse_php.Parse_error _err ->
      Common.pr2 (spf "warning: parsing problem in %s" file);
      [])
  | _ ->
    Fuzzy
    (try
      (match !lang with
      | ("c" | "c++") ->
        Common.save_excursion Flag_parsing_cpp.verbose_lexing false (fun () ->
          Parse_cpp.parse_fuzzy file +> fst
        )
      | "java" ->
        Parse_java.parse_fuzzy file +> fst
      | "js" ->
        Parse_js.parse_fuzzy file +> fst
      | "ml" ->
        Parse_ml.parse_fuzzy file +> fst
      | "phpfuzzy" ->
        Parse_php.parse_fuzzy file +> fst
      | _ ->
        failwith ("unsupported language: " ^ !lang))
    with exn ->
      pr2 (spf "PB with %s, exn = %s"  file (Common.exn_to_s exn));
      [])

let parse_pattern str =
  match !lang with
  | "php" -> Left (Sgrep_php.parse str)
  (* for now we abuse the fuzzy parser of cpp for ml for the pattern as
   * we should not use comments in patterns
   *)
  | "c" | "c++" | "ml" | "java" | "js" | "phpfuzzy" -> 
    Right (ast_fuzzy_of_string str)
  | _ -> failwith ("unsupported language: " ^ !lang)

let read_patterns name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop ((parse_pattern s) :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let sgrep_ast pattern any_ast =
  match !lang, pattern, any_ast with
  | ("c" | "c++"), Right pattern, Fuzzy ast ->
    Sgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast
  | "java", Right pattern, Fuzzy ast ->
    Sgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast
  | "js", Right pattern, Fuzzy ast ->
    Sgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast
  | "ml", Right pattern, Fuzzy ast ->
    Sgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast
  | "php", Left pattern, Php ast ->
    Sgrep_php.sgrep_ast
      ~case_sensitive:!case_sensitive
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Lib_parsing_php.ii_of_any matched_tokens
      )
      pattern ast
  | "phpfuzzy", Right pattern, Fuzzy ast ->
    Sgrep_fuzzy.sgrep
      ~hook:(fun env matched_tokens ->
        print_match !mvars env Ast_fuzzy.toks_of_trees matched_tokens
      )
      pattern ast
  | _ ->
    failwith ("unsupported language: " ^ !lang)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action xs =
  let patterns, query_string =
    match !pattern_file, !pattern_string, !use_multiple_patterns with
    | "", "", _ ->
        failwith "I need a pattern; use -f or -e"
    | file, _, true when file <> "" ->
        read_patterns file, "multi"
    | file, _, _ when file <> "" ->
        let s = Common.read_file file in
        [parse_pattern s], s
    | _, s, true when s <> ""->
        failwith "cannot combine -multi with -e"
    | _, s, _ when s <> ""->
        [parse_pattern s], s
    | _ -> raise Impossible
  in
  Logger.log Config_pfff.logger "sgrep" (Some query_string);

  let files = 
    Find_source.files_of_dir_or_files ~lang:!lang ~verbose:!verbose xs in

  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);
    let ast = create_ast file in
    let sgrep pattern = sgrep_ast pattern ast in
    List.iter sgrep patterns
  );

  !layer_file +> Common.do_option (fun file ->
    let root = Common2.common_prefix_of_files_or_dirs xs in
    gen_layer ~root ~query:query_string  file
  );
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)
let dump_sgrep_php_pattern file =
  let any = Parse_php.parse_any file in
  let s = Export_ast_php.ml_pattern_string_of_any any in
  pr s

(*---------------------------------------------------------------------------*)
(* Regression testing *)
(*---------------------------------------------------------------------------*)
open OUnit
let test () =
  let suite = "sgrep" >::: [
   (* ugly: todo: use a toy fuzzy parser instead of the one in lang_cpp/ *)
    Unit_matcher.sgrep_unittest ~ast_fuzzy_of_string;
    Unit_matcher_php.sgrep_unittest;
  ]
  in
  OUnit.run_test_tt suite +> ignore;
  ()

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let sgrep_extra_actions () = [
(*
  "-dump_php_pattern", " <file> (internal)",
  Common.mk_action_1_arg dump_sgrep_php_pattern;
*)
  "-test", " run regression tests",
  Common.mk_action_0_arg test;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 sgrep_extra_actions()@
 []

let options () = 
  [
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);

    "-e", Arg.Set_string pattern_string, 
    " <pattern> expression pattern";
    "-f", Arg.Set_string pattern_file, 
    " <file> obtain pattern from file";
    "-multi", Arg.Set use_multiple_patterns,
    " combine with -f <file> to obtain multiple patterns from file, one per line";

    "-case_sensitive", Arg.Set case_sensitive, 
    " match code in a case sensitive manner";

    "-emacs", Arg.Unit (fun () -> match_format := Matching_report.Emacs ),
    " print matches on the same line than the match position";
    "-oneline", Arg.Unit (fun () -> match_format := Matching_report.OneLine),
    " print matches on one line, in normalized form";

    "-pvar", Arg.String (fun s -> mvars := Common.split "," s),
    " <metavars> print the metavariables, not the matched code";

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in a pfff layer file\n";

    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      Flag_matcher.verbose := true;
      Flag_matcher_php.verbose := true;
    ),
    " ";
  ] @
  (* old: Flag_parsing_php.cmdline_flags_pp () ++ *)
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "sgrep version: %s" Config_pfff.version);
    exit 0;
  ), 
    "  guess what";
  ] @
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
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
