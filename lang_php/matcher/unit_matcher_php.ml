open Common

open Ast_php
module Ast = Ast_php

open OUnit

module Flag = Flag_parsing_php

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "matcher_php" >::: [
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



    );
  ]

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
    "-unittest_matcher", "   ", 
    Common.mk_action_0_arg (fun () -> OUnit.run_test_tt unittest |> ignore);
]
