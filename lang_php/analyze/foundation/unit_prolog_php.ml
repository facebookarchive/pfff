open Common

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * What to put here? Should we duplicate things from unit_static_analysis_php
 * as many results from static analysis are now translated into prolog
 * facts? No, no need to duplicate, just copy here the basic
 * versions of some tests, e.g. for the callgraph just the basic
 * function and method calls for instance.
 * 
 * todo: port most of unit_analyze_db_php.ml here using also the abstract
 * interpreter for more "demanding" callgraph/datagraph unit tests.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo? could perhaps be moved in its own database_prolog.ml file? *)
let prolog_query ~file query =
  let source_file = Parse_php.tmp_php_file_from_string file in
  let facts_pl_file = Common.new_temp_file "prolog_php_db" ".pl" in
  let helpers_pl_file = Config.path ^ "/h_program-lang/database_code.pl" in

  (* make sure it's a valid PHP file *)
  let _ast = Parse_php.parse_program source_file in

  (* todo: at some point avoid using database_php_build and 
   * generate the prolog db directly from the sources (with
   * also the callgraph info of the abstract interpreter)
   *)
  let db = Database_php_build.db_of_files_or_dirs [source_file] in
  Database_prolog_php.gen_prolog_db 
    ~show_progress:false db facts_pl_file;
  Database_prolog_php.append_callgraph_to_prolog_db
    ~show_progress:false db facts_pl_file;

  (* debug: Common.cat facts_pl_file +> List.iter pr2; *)
  Common.cat facts_pl_file +> List.iter pr2;
  let cmd = 
    spf "swipl -s %s -f %s -t halt --quiet -g \"%s ,fail\""
      facts_pl_file helpers_pl_file query
  in
  let xs = Common.cmd_to_list cmd in
  xs

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "prolog" >::: (
    [

    (*-----------------------------------------------------------------------*)
    (* Entities *)
    (*-----------------------------------------------------------------------*)
    "kinds" >:: (fun () ->
      let file = "
function foo() { }
const BAR = 1;
class A { }
interface I { }
trait T { }
class B { const CST = 1; }
" in 
      (* quite similar to the unit test for tags in unit_foundation_php.ml *)
      assert_equal 
        ["function"]  (prolog_query ~file "kind('foo', X), writeln(X)");
      assert_equal 
        ["constant"]  (prolog_query ~file "kind('BAR', X), writeln(X)");
      assert_equal 
        ["class"]     (prolog_query ~file "kind('A', X), writeln(X)");
      assert_equal 
        ["interface"] (prolog_query ~file "kind('I', X), writeln(X)");
      assert_equal 
        ["trait"]     (prolog_query ~file "kind('T', X), writeln(X)");
      assert_equal 
        ["class_constant"]
          (prolog_query ~file "kind(('B','CST'), X), writeln(X)");
    );

    (*-----------------------------------------------------------------------*)
    (* Inheritance *)
    (*-----------------------------------------------------------------------*)

    "inheritance" >:: (fun () ->
      let file = "
class A { }
class B extends A { }
class C extends B { }
"
      in
      let xs = prolog_query ~file "children(X, 'A'), writeln(X)" in
      assert_equal ~msg:"it should find all children of a class"
        (sort ["B";"C"])
        (sort xs)
    );

    (*-----------------------------------------------------------------------*)
    (* Traits *)
    (*-----------------------------------------------------------------------*)

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
      let xs = prolog_query ~file "method('A', (_Class, X)), writeln(X)"
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

    (*-----------------------------------------------------------------------*)
    (* Privacy and inheritance *)
    (*-----------------------------------------------------------------------*)
    (* todo: tricky when traits *)

    (*-----------------------------------------------------------------------*)
    (* Override *)
    (*-----------------------------------------------------------------------*)

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

    (*-----------------------------------------------------------------------*)
    (* Callgraph *)
    (*-----------------------------------------------------------------------*)
    "callgraph" >:: (fun () ->
      let file = "
function foo() { }
function bar() { foo(); }
" in
      let xs = prolog_query ~file 
        "docall(X, 'foo', function), writeln(X), fail" in
      assert_equal ~msg:"it should find basic callers to a function"
        ["bar"]
        (sort xs);

      (* this one requires more sophisticated analysis *)
      let file ="
class A { function foo() { } }
class B extends A { }
function bar() {
  $o = new B();
  $y = $o->foo();
} " in
      let xs = prolog_query ~file 
        "docall('bar', (X,Y), method), writeln((X,Y)), fail" in
      assert_equal ~msg:"it should find basic callers to a function"
        ["A,foo"]
        (sort xs);
      

    );

    (*-----------------------------------------------------------------------*)
    (* XHP *)
    (*-----------------------------------------------------------------------*)
    "xhp" >:: (fun () ->
      let file = "
class :x:frag {
 attribute string template;
}
"in
      let xs = prolog_query ~file "field(':x:frag', (_, X)), writeln(X)" in
      assert_equal ~msg:"it should understand xhp attributes"
        (["template"])
        xs
    );

    (* todo: handle also children, inherit, etc *)

  ]
  )
