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
  let test_files = [
    "tests/php/scheck/includes.php";
    "tests/php/scheck/variables.php";
    "tests/php/scheck/variables_fp.php";
    "tests/php/scheck/functions.php";
    "tests/php/scheck/static_methods.php";
    "tests/php/scheck/methods.php";
    "tests/php/scheck/classes.php";
    "tests/php/scheck/traits.php";
    "tests/php/scheck/cfg.php";
    "tests/php/scheck/references.php";
    "tests/php/scheck/endpoint.php";
    "tests/php/scheck/misc.php";
  ] 
  in
  let test_files = 
    test_files +> List.map (fun s -> Filename.concat Config.path s) in
  let php_stdlib =
    Filename.concat Config.path "/data/php_stdlib" in
  
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
  let builtin_files =
    Lib_parsing_php.find_php_files_of_dir_or_files [php_stdlib]
  in

  Error_php._errors := [];
  let db = 
    Database_php_build.db_of_files_or_dirs (builtin_files ++ test_files) in
  let find_entity = 
    Some (Database_php_build.build_entity_finder db) in
  let env = 
    Env_php.mk_env ~php_root:"/" in

  let verbose = false in

  (* run the bugs finders *)
  test_files +> List.iter (fun file ->
    Check_all_php.check_file ~verbose ~find_entity env file
  );
  if verbose then begin
    !Error_php._errors +> List.iter (fun e -> 
      pr (Error_php.string_of_error e));
  end;
  
  let (actual_errors: (Common.filename * int (* line *)) list) = 
    !Error_php._errors +> Common.map (fun err ->
      let info = err.Error_php.loc in
      Ast.file_of_info info, Ast.line_of_info info
      )
  in
  
  (* diff report *)
  let (common, only_in_expected, only_in_actual) = 
    Common.diff_set_eff expected_errors actual_errors in

  only_in_expected |> List.iter (fun (src, l) ->
    pr2 (spf "this one error is missing: %s:%d" src l);
  );
  only_in_actual |> List.iter (fun (src, l) ->
    pr2 (spf "this one error was not expected: %s:%d" src l);
  );
  assert_bool
    ~msg:"it should find all reported errors and no more"
    (null only_in_expected && null only_in_actual);
  )
  ]
