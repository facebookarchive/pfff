open Common

module Ast = Ast_php
module A = Annotation_php

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Defs/uses *)
(*---------------------------------------------------------------------------*)

let defs_uses_unittest =
  "defs_uses" >::: [
    "functions uses" >:: (fun () ->
      let file_content = "
foo1();
"
      in
      let ast = Parse_php.program_of_string file_content in
      let uses = Defs_uses_php.uses_of_any (Ast.Program ast) in
      let uses_strings = 
        uses +> List.map (fun (kind, name) -> Ast.name name) in
      assert_equal 
        (sort ["foo1"])
        (sort uses_strings);
    );

    "classes uses" >:: (fun () ->
      let file_content = "
new Foo1();
if($x instanceof Foo2) { }

echo Foo3::Cst;
echo Foo4::$var;
Foo5::method();
Foo6::$f();

class X1 extends Foo7 { }
class X2 implements Foo8 { }
try { } catch(Foo9 $x) { }
function foo1(Foo10 $x) { }

$x = <x:xhp1></x:xhp1>;
$x = <x:xhp2/>;
"
      in
      let ast = Parse_php.program_of_string file_content in
      let uses = Defs_uses_php.uses_of_any (Ast.Program ast) in
      let str_of_name = function
        | Ast.Name (s, _) -> s
        | Ast.XhpName (xhp_tag, _) ->
            Common.join ":" xhp_tag
      in
      let uses_strings = 
        uses +> List.map (fun (kind, name) -> str_of_name name) in

      let classes = 
        (Common.enum 1 10) +> List.map (fun i -> spf "Foo%d" i) in
      let xhp_classes = 
        (Common.enum 1 2) +> List.map (fun i -> spf "x:xhp%d" i) in
      assert_equal 
        (sort (classes ++ xhp_classes))
        (sort uses_strings);
    );
  ]

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
            trait C { }
        "
        in
        let tmpfile = Parse_php.tmp_php_file_from_string file_content in
        let tags = 
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            assert_equal 
              ~msg:"The tags should contain only 5 entries"
              (List.length tags_in_file) 5;
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
        let tmpfile = Parse_php.tmp_php_file_from_string file_content in
        let tags = 
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            (* we used to generate 2 tags per method, one for 'a_method',
             * and one for 'A::a_method', but if there is also somewhere
             * a function called a_method() and that it's located in an
             * alphabetically higher filenames, then M-. a_method
             * will unfortunately go the method. So just simpler to not
             * generate the a_method tag.
             *)
            assert_equal 
              ~msg:"The tags should contain only 2 entries"
              (List.length tags_in_file) 2;
        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
      );

      "xhp tags" >:: (fun () ->
        let file_content = "class :x:foo { }" in
        let tmpfile = 
          Parse_php.tmp_php_file_from_string file_content in
        let tags = 
          Tags_php.php_defs_of_files_or_dirs ~verbose:false [tmpfile] in
        let all_tags = tags +> List.map snd +> List.flatten in
        assert_bool
          ~msg:"it should contain an entry for the :x:... classname form"
          (all_tags +> List.exists (fun t -> 
            t.Tags_file.tagname =$= ":x:foo"));
        assert_bool
          ~msg:"it should contain an entry for the x:... classname form"
          (all_tags +> List.exists (fun t -> 
            t.Tags_file.tagname =$= "x:foo"));

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
      let tmpfile = Parse_php.tmp_php_file_from_string file_content in
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
(* Include use/def *)
(*---------------------------------------------------------------------------*)

let include_unittest =
  "include_require" >::: (
      let env = {
        Env_php.global_arrays = Common.hash_of_list [
          "_SERVER", Common.hash_of_list [
            "PHP_ROOT", "/home/foo/www";
          ];
        ];
        Env_php.constants = Common.hash_of_list [];
        Env_php.globals = Common.hash_of_list [];
        Env_php.globals_specials = (fun s dir -> None);
      }
      in [
      (* I was previously using Filename.concat in include_require_php.ml
       * which generate paths like a/b//c/foo.php which is annoying
       * and can confuse some of our analysis. Check that no regression
       * on this issue.
       *)
      "resolving path" >:: (fun () ->
        let file = "
        require_once $_SERVER['PHP_ROOT'].'/lib/alerts/alerts.php';
        "
        in
        let ast = Parse_php.program_of_string file in
        let incs = Include_require_php.top_increq_of_program ast in
        match incs with
        | [(_inc_kind,_tok, incexpr)] ->
            let path =
              Include_require_php.resolve_path (env, "/") incexpr in
            assert_equal
              (Some "/home/foo/www/lib/alerts/alerts.php")
              path;

        | _ ->
            assert_failure
              "wrong number of elements returned by increq_of_program"
      );

      ])

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "foundation_php" >::: [
    defs_uses_unittest;
    tags_unittest;
    annotation_unittest;
    include_unittest;
  ]
