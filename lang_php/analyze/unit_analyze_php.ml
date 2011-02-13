open Common

module Ast = Ast_php

module Db = Database_php
module Cg = Callgraph_php

module V = Visitor_php
module A = Annotation_php

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* It is clearer for our testing code to programmatically build source files
 * so that all the information about a test is in the same
 * file. You don't have to open extra files to understand the test
 * data.
 *)
let tmp_php_file_from_string s =
  let tmp_file = Common.new_temp_file "test" ".php" in
  Common.write_file ~file:tmp_file ("<?php\n" ^ s);
  tmp_file


(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Tags *)
(*---------------------------------------------------------------------------*)
let tags_unittest =
    "tags_php" >::: [
      "basic tags" >:: (fun () ->
        let file_content = "
            function foo() { } 
            class A { }
            define('Cst',1);
            interface B { }
        "
        in
        let tmpfile = tmp_php_file_from_string file_content in
        let tags = 
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            assert_equal 
              ~msg:"The tags should contain only 4 entries"
              (List.length tags_in_file) 4;
        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
      );
      "method tags" >:: (fun () ->
        let file_content = "
           class A {
              function a_method() { } 
           }
        " in
        let tmpfile = tmp_php_file_from_string file_content in
        let tags = 
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            (* we generate 2 tags per method, one for a_method, and one for
             * A::a_method
             *)
            assert_equal 
              ~msg:"The tags should contain only 3 entries"
              (List.length tags_in_file) 3;
        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
      );
    ]

(*---------------------------------------------------------------------------*)
(* Annotations *)
(*---------------------------------------------------------------------------*)

let annotation_unittest =
  "annotation_php" >::: [
    "data provider annotations" >:: (fun () ->
      let file_content = "
        class A {
          // @dataProvider provider
          public function foo() { }
          public function provider() { }
          // @dataProvider B::provider2
          public function foo2() { }
          /**
           * @dataProvider provider3
           */
          public function foo3() { }
          /*
           * @dataProvider provider4
           */
          public function foo4() { }
}
"
      in
      let tmpfile = tmp_php_file_from_string file_content in
      let (ast_with_comments, _stat) = Parse_php.parse tmpfile in
      let annots = 
        Annotation_php.annotations_of_program_with_comments ast_with_comments
          +> List.map snd
      in
      assert_equal ~msg:"should have the DataProvider annotations"
        (sort [A.DataProvider (A.Method "provider");
               A.DataProvider (A.MethodExternal ("B", "provider2"));
               A.DataProvider (A.Method "provider3");
               A.DataProvider (A.Method "provider4");
        ])
        (sort annots);
    );
  ]

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "analyze_php" >::: [
    tags_unittest;
    annotation_unittest;
  ]
