open Common
open OUnit

module Ast = Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
 "checkers_php" >::: [
  "basic checkers" >:: (fun () ->
  let p path = Filename.concat Config_pfff.path path in

  let test_files = [
    p "tests/php/scheck/builtins.php";
    p "tests/php/scheck/common.php";

    p "tests/php/scheck/includes.php";

    p "tests/php/scheck/variables.php";
    p "tests/php/scheck/variables_fp.php";
    p "tests/php/scheck/arrays.php";
    p "tests/php/scheck/foreach.php";
    p "tests/php/scheck/edit_distance.php";

    p "tests/php/scheck/functions.php";
    p "tests/php/scheck/static_methods.php";
    p "tests/php/scheck/methods.php";

    p "tests/php/scheck/classes.php";
    p "tests/php/scheck/traits.php";
(*
    p "tests/php/scheck/namespaces.php";
    p "tests/php/scheck/namespaces_uses.php";
*)

    p "tests/php/scheck/cfg.php";
    p "tests/php/scheck/references.php";
    p "tests/php/scheck/xhp.php";
    p "tests/php/scheck/typing.php";

    p "tests/php/scheck/dynamic_bailout.php";

    p "tests/php/scheck/format_string.php";
    p "tests/php/scheck/ternary_if.php";
    p "tests/php/scheck/misc.php";

    p "tests/php/scheck/lint.php";
    p "tests/php/scheck/micro_clones.php";
  ] 
  in

  (* old: Lib_parsing_php.find_php_files_of_dir_or_files [p "/data/php_stdlib"]
   * we need a tests/php/scheck/php_stdlib.php because data/php_stdlib
   * is now mostly auto generated from idl files, and data/php_stdlib
   * contains only pfff internal builtins.
   * data/php_stdlib is not anymore in the pfff repo.
   *)
  let builtin_files = [
    p "tests/php/scheck/php_stdlib.php";
  ]

  in
  let files = builtin_files @ test_files in

  let (expected_errors :(Common.filename * int (* line *)) list) =
    test_files +> List.map (fun file ->
      Common.cat file +> Common.index_list_1 +> Common.map_filter 
        (fun (s, idx) -> 
          (* Right now we don't care about the actual error messages. We
           * don't check if they match. We are just happy to check for 
           * correct lines error reporting.
           *)
          if s =~ ".*//ERROR:.*" 
          (* + 1 because the comment is one line before *)
          then Some (file, idx + 1) 
          else None
        )
    ) +> List.flatten
  in
  Error_php._errors := [];

  let verbose = false in

  (* old:
   *  let db = Database_php_build.db_of_files_or_dirs files in
   *  let find_entity = Some (Database_php_build.build_entity_finder db) in
   *)
  let (cg, _stat) = Graph_code_php.build ~verbose "/" files in
  let find_entity = 
    Some (Entity_php.entity_finder_of_graph_code ~check_dupes:true
             cg "/") in
  
  let env = Env_php.mk_env ~php_root:"/" in

  (* run the bugs finders *)
  test_files +> List.iter (fun file ->
    Check_all_php.check_file ~verbose ~find_entity env file;
    Check_classes_php.check_required_field cg file
  );
  if verbose then begin
    !Error_php._errors +> List.iter (fun e -> pr (Error_php.string_of_error e))
  end;
  
  let (actual_errors: (Common.filename * int (* line *)) list) = 
    !Error_php._errors +> List.map (fun err ->
      let info = err.Error_php.loc in
      Parse_info.file_of_info info, Parse_info.line_of_info info
      )
  in
  
  (* diff report *)
  let (_common, only_in_expected, only_in_actual) = 
    Common2.diff_set_eff expected_errors actual_errors in

  only_in_expected +> List.iter (fun (src, l) ->
    pr2 (spf "this one error is missing: %s:%d" src l);
  );
  only_in_actual +> List.iter (fun (src, l) ->
    pr2 (spf "this one error was not expected: %s:%d (%s)" src l
           (!Error_php._errors +> List.find (fun err ->
             let info = err.Error_php.loc in
             src =$= Parse_info.file_of_info info &&
             l   =|= Parse_info.line_of_info info
            ) +> Error_php.string_of_error));
  );
  assert_bool
    ~msg:(spf "it should find all reported errors and no more (%d errors)"
             (List.length (only_in_actual @ only_in_expected)))
    (null only_in_expected && null only_in_actual);
  )
  ]
