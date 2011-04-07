open Common

open Ast_php
module Ast = Ast_php

open OUnit

module Flag = Flag_parsing_php

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* run by sgrep -test *)
let sgrep_unittest = [
  "sgrep variable metavars matching" >:: (fun () ->

    let pattern = Parse_php.any_of_string "foo($V, $V);" in
    let code = Parse_php.any_of_string "foo($x, $y);" in
    (match pattern, code with
    | Stmt2 pattern, Stmt2 code ->
        let matches_with_env = Matching_php.match_st_st pattern code in
        assert_bool "should not match" (matches_with_env = []);
    | _ ->
        assert_failure "parsing problem in sgrep pattern parsing"
    );

    let pattern = Parse_php.any_of_string "foo($V, $V);" in
    let code = Parse_php.any_of_string "foo($x, $x);" in
    (match pattern, code with
    | Stmt2 pattern, Stmt2 code ->
        let matches_with_env =  Matching_php.match_st_st pattern code in
        assert_bool "should match" (matches_with_env <> []);
    | _ ->
        assert_failure "parsing problem in sgrep pattern parsing"
    );
  )
]

(* run by spatch -test *)
let spatch_unittest = [
  "spatch regressions" >:: (fun () ->

    let testdir = Filename.concat Config.path "tests/php/spatch/" in
    let expfiles = Common.glob (testdir ^ "*.exp") in
  
    expfiles +> List.iter (fun expfile ->
      (* todo: this regexp should just be .*? but ocaml regexp do not
       * have the greedy feature :( Also note that expfile is a fullpath
       * so it can contains /, hence this ugly regexp
       *)
      if expfile =~ "\\([a-zA-Z_/]+\\)\\([0-9]*\\)\\.exp$" then begin
        let (prefix, variant) = Common.matched2 expfile in
        let spatchfile = prefix ^ ".spatch" in
        let phpfile = prefix ^ variant ^ ".php" in
        pr2 (spf "Testing %s on %s expecting %s" 
                (Filename.basename spatchfile)
                (Filename.basename phpfile)
                (Filename.basename expfile));
        
        let pattern = Spatch_php.parse_spatch spatchfile in
        let resopt = Spatch_php.spatch pattern phpfile in
        
        let file_res = 
          match resopt with
          | None -> phpfile
          | Some s ->
              let tmpfile = Common.new_temp_file "spatch_test" ".php" in
              Common.write_file ~file:tmpfile s;
              tmpfile
        in
        let diff = Common.cmd_to_list (spf "diff -u %s %s" file_res expfile) in
        diff |> List.iter pr;
        if List.length diff > 1
        then failwith (spf "PB with %s" expfile);
      end 
      else failwith ("wrong format for expfile: " ^ expfile)
    )
  )
]
(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)

let unittest =
  "matcher_php" >::: (
    sgrep_unittest ++ spatch_unittest
  )

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
    "-unittest_matcher", "   ", 
    Common.mk_action_0_arg (fun () -> OUnit.run_test_tt unittest |> ignore);
]
