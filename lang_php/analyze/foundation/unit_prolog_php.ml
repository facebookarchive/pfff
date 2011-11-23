open Common

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo: port most of unit_analyze_db_php.ml here using also the abstract
 * interpreter for more "demanding" callgraph/datagraph unit tests.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo? could perhaps be moved in its own database_prolog.ml file? *)
let prolog_query ~file query =
  let source_file = Common.new_temp_file "prolog_php" ".php" in
  let facts_pl_file = Common.new_temp_file "prolog_php_db" ".pl" in
  let helpers_pl_file = Config.path ^ "/h_program-lang/database_code.pl" in

  Common.write_file ~file:source_file ("<?php\n" ^ file);
  (* make sure it's a valid PHP file *)
  let _ast = Parse_php.parse_program source_file in
  let db = Database_php_build.db_of_files_or_dirs [source_file] in
  Database_prolog_php.gen_prolog_db db facts_pl_file;
  let cmd = 
    spf "swipl -s %s -f %s -t halt --quiet -g \"%s\""
      facts_pl_file helpers_pl_file query
  in
  let xs = Common.cmd_to_list cmd in
  xs

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "prolog" >::: ([

    "inheritance" >:: (fun () ->
      let file = "
class A { }
class B extends A { }
class C extends B { }
"
      in
      let xs = prolog_query ~file "children(X, 'A'), writeln(X), fail" in
      assert_equal ~msg:"it should find all children of a class"
        (sort ["B";"C"])
        (sort xs)
    );

    "traits" >:: (fun () ->
      let file = "
trait T { 
  public function trait1() { } 
}
class A { 
  use T; 
  public function a() { } 
}
"
      in
      let xs = prolog_query ~file "method('A', (_Class, X)), writeln(X), fail"
      in
      assert_equal ~msg:"it should find all methods of a class using traits"
        (sort ["a";"trait1"])
        (sort xs);

    let file = "
trait T1 { public function foo() { } }
trait T2 { public function bar() { } }
trait TComp { use T1, T2; }
class A { use TComp; } 
"
    in
    let xs = prolog_query ~file "method('A', (_Class, X)), writeln(X), fail"
    in
    assert_equal 
      ~msg:"it should find all methods of a class using multiple traits"
        (sort ["foo";"bar"])
        (sort xs)
    );

    "overrides" >:: (fun () ->
      let file = "
class A { 
   public function foo() { } 
   public function bar() { } 
}
class B extends A { public function foo() { } }
"
      in
      let xs = prolog_query ~file 
        "overrides(Class, Method), writeln(Method), fail" in
      assert_equal ~msg:"it should detect overriden methods"
        (sort ["foo"])
        (sort xs);

    );
  ]
  )
