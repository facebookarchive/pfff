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

(* A lint-like checker for PHP.
 * https://github.com/facebook/pfff/wiki/Scheck
 * 
 * By default 'scheck' performs only a local analysis of the files passed
 * on the command line. It is thus quite fast while still detecting a few
 * important bugs like the use of undefined variables. 
 * 
 * 'scheck' can also leverage more expensive global analysis to find more bugs.
 * Doing so requires a PHP code database which is usually very expensive 
 * to build (see pfff_db_heavy) and takes lots of space. Fortunately one can
 * now build this database in memory, on the fly. Indeed, thanks
 * to the include_require_php.ml analysis, we can now
 * build only the db for the files that matters, cutting significantly
 * the time to build the db (going down from 40 000 files to about 1000
 * files on average on facebook code). In a way it is similar
 * to what gcc does when it calls 'cpp' to get the full information for
 * a file. 
 * 
 * 'scheck' can also leverage a light database (see pfff_db) and 
 * use this as a cache.
 * 
 * 'scheck' could also use the heavy database but this requires to have
 * the program linked with Berkeley DB, adding some dependencies to 
 * the user of the program (and is not very multi-user friendly for now).
 * See main_scheck_heavy.ml for such a program.
 * 
 * Note that scheck is mostly for generic bugs (that sometimes
 * requires global analysis). For API-specific bugs, you can use 'sgrep'.
 * 
 * modes:
 *  - local analysis
 *  - perform global analysis "lazily" by building db on-the-fly
 *    of the relevant included files (configurable via a -depth_limit flag)
 *  - leverage global analysis computed previously by codegraph
 *  - TODO leverage global analysis computed previously by pfff_db(light)
 *  - leverage global analysis computed by pfff_db_heavy, 
 *    see main_scheck_heavy.ml
 * 
 * current checks:
 *   - variable related (use of undeclared variable, unused variable, etc)
 *     with good handling of reference false positives when have a code db
 *   - use/def of entities (e.g. use of undefined class/function/constant
 *     a la checkModule)
 *   - function call related (wrong number of arguments, bad keyword
 *     arguments, etc)
 *   - SEMI class related (use of undefined member, wrong number of arguments
 *     in method call, etc)
 *   - include/require and file related (e.g. including file that do not
 *     exist anymore); needs to pass an env
 *   - SEMI dead code (dead function in callgraph, DONE dead block in CFG,
 *     dead assignement in dataflow)
 *   - TODO type related
 *   - TODO resource related (open/close match)
 *   - TODO security related? via sgrep?
 *   - TODO require_strict() related (see facebook/.../main_linter.ml)
 * 
 * related: 
 *   - TODO lint_php.ml (small syntactic conventions, e.g. bad defines)
 *   - TODO check_code_php.ml (include/require stuff)
 *   - TODO check_module.ml (require_module() stuff), 
 *   - TODO main_linter.ml (require_strict() stuff), 
 *   - TODO main_checker.ml (flib-aware  checker),
 * 
 * todo: make it possible to take a db in parameter so
 * for other functions, we can also get their prototype.
 * 
 * The checks leverage also info about builtins, so when one calls preg_match(),
 * we know that this function takes things by reference which avoids
 * some false positives regarding the use of undeclared variable
 * for instance.
 * 
 * later: it could later also check javascript, CSS, sql, etc
 * 
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

(* In strict mode, we are more aggressive regarding scope like in
 * JsLint. This is a copy of the same variable in Error_php.ml
 *)
let strict_scope = ref false 

(* running the heavy analysis processing for instance the included files *)
let heavy = ref false

(* depth_limit is used to stop the expensive recursive includes process.
 *
 * I put 5 because it's fast enough at depth 5, and 
 * I think it's good enough as it is probably bad for a file to use
 * something that is distant by more than 5 includes. 
 * 
 * todo: one issue is that some code like facebook uses special 
 *  require/include directives that include_require_php.ml is not aware of.
 *  Maybe we should have a unfacebookizer preprocessor that removes
 *  this sugar. The alternative right now is to copy most of the code
 *  in this file in facebook/qa_code/checker.ml :( and plug in the
 *  special include_require_php.ml hooks. Another alternative is to use
 *  the light_db.json cache.
 *)
let depth_limit = ref (Some 5: int option)

let php_stdlib = 
  ref (Filename.concat Config_pfff.path "/data/php_stdlib")

let cache_parse = ref true

(* no -heavy or -depth_limit or -php_stdlib or -cache_parse here *)
(* old: main_scheck_heavy: let metapath = ref "/tmp/pfff_db" *)

(* for ranking errors *)
let rank = ref true

(* for codemap or layer_stat *)
let layer_file = ref (None: filename option)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Build the database of information. Take the name of the file
 * we want to check and process all the files that are needed (included)
 * to check it, a la cpp.
 * 
 * Some checks needs to have a global view of the code, for instance
 * to know what are the sets of valid protected variable that can be used
 * in a child class.
 * 
 * todo: can probably optimize this later. For instance lazy
 * loading of files, stop when are in flib as modules are
 * not transitive.
 * 
 * see also facebook/.../dependencies.ml
 *)
let build_mem_db file =

  (* todo: could infer PHPROOT at least ? just look at
   * the include in the file and see where the files are.
   *)
  let env = 
    Env_php.mk_env (Common2.dirname file)
  in
  let root = "/" in (* todo ? *)

  let all_files = 
    Include_require_php.recursive_included_files_of_file 
      ~verbose:!verbose 
      ~depth_limit:!depth_limit
      env file
  in
  let builtin_files =
    Lib_parsing_php.find_php_files_of_dir_or_files [!php_stdlib]
  in
  Common.save_excursion Flag_analyze_php.verbose_database !verbose (fun()->
    Database_php_build.create_db
      ~db_support:(Database_php.Mem)
      ~phase:2 (* TODO ? *)
      ~files:(Some (builtin_files ++ all_files))
      ~verbose_stats:false
      ~annotate_variables_program:None
      (Database_php.prj_of_dir root) 
  )

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_dbg s =
  if !verbose then Common.pr2 s

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  Logger.log Config_pfff.logger "scheck" None;

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
  let errors = ref [] in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;
  Error_php.strict := !strict_scope;

  Common.save_excursion Flag_parsing_php.caching_parsing !cache_parse (fun ()->
  files +> List.iter (fun file ->
    try 
      (*TODO: use Common_extra.with_progress *)
      pr2_dbg (spf "processing: %s" file);
      let find_entity =
(*
  Database_php.with_db ~metapath:!metapath (fun db ->
  let find_entity = Some (Database_php_build.build_entity_finder db) in
*)
        if not !heavy 
        then None
        else 
          let db = build_mem_db file in
          Some (Database_php_build.build_entity_finder db) 
      in
      let env = 
        Env_php.mk_env (Common2.dirname file)
      in
      Check_all_php.check_file ~find_entity env file
    with 
    | (Timeout | UnixExit _) as exn -> raise exn
(*    | (Unix.Unix_error(_, "waitpid", "")) as exn -> raise exn *)
    | exn ->
        Common.push2 (spf "PB with %s, exn = %s" file 
                         (Common.exn_to_s exn)) errors;
        if !Common.debugger then raise exn
  ));

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
    let root = Common2.common_prefix_of_files_or_dirs xs in

    Layer_checker_php.gen_layer ~root ~output:file !Error_php._errors
  );
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* type inference playground *)
(*---------------------------------------------------------------------------*)

let type_inference file =
  raise Todo
(*

  let ast = Parse_php.parse_program file in

  (* PHP Intermediate Language *)
  try
    let pil = Pil_build.pil_of_program ast in

    (* todo: how bootstrap this ? need a bottom-up analysis but
     * we could first start with the types of the PHP builtins that
     * we already have (see builtins_php.mli in lang_php/analyze/).
     *)
    let env = () in

    (* works by side effect on the pil *)
    Type_inference_pil.infer_types env pil;

    (* simple pretty printer *)
    let s = Pretty_print_pil.string_of_program pil in
    pr s;

    (* internal representation pretty printer *)
    let s = Meta_pil.string_of_program
      ~config:{Meta_pil.show_types = true; show_tokens = false}
      pil
    in
    pr s;

  with exn ->
    pr2 "File contain constructions not supported by the PIL; bailing out";
    raise exn
*)

(*---------------------------------------------------------------------------*)
(* Regression testing *)
(*---------------------------------------------------------------------------*)
let test () =
  let suite = Unit_checker_php.unittest in
  OUnit.run_test_tt suite +> ignore;
  ()

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
  "-type_inference", " <file>",
  Common.mk_action_1_arg type_inference;
  "-test", " ",
  Common.mk_action_0_arg test;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
 extra_actions()++
 []

let options () =
  [
    "-verbose", Arg.Set verbose,
    " guess what";

    "-heavy", Arg.Set heavy,
    " process included files";
    "-depth_limit", Arg.Int (fun i -> depth_limit := Some i), 
    " limit the number of includes to process";
    "-no_caching", Arg.Clear cache_parse, 
    " don't cache parsed ASTs";
     "-php_stdlib", Arg.Set_string php_stdlib, 
     (spf " path to builtins (default = %s)" !php_stdlib);

    "-strict", Arg.Set strict_scope,
    " emulate block scope instead of function scope";
    "-no_scrict", Arg.Clear strict_scope, 
    " use function scope (default)";

    "-no_rank", Arg.Clear rank,
    " ";

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in pfff layer file";

  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common2.cmdline_flags_devel () ++
  [
  "-version",   Arg.Unit (fun () ->
    pr2 (spf "scheck version: %s" Config_pfff.version);
    exit 0;
  ),
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () ->
    pr2 "version: $Date: 2011/05/12 00:44:57 $";
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

  let usage_msg =
    "Usage: " ^ Common2.basename Sys.argv.(0) ^
      " [options] <file or dir> " ^ "\n" ^ "Options are:" ^
      "https://github.com/facebook/pfff/wiki/Scheck"
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
