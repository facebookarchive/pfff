open Common

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * What to put here? Should we duplicate things from unit_static_analysis_php
 * as many results from static analysis are now translated into Prolog
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

let prolog_query ~file query =
  let source_file = Parse_php.tmp_php_file_from_string file in
  Database_prolog_php.prolog_query ~verbose:false ~source_file ~query

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

      (* this one requires more sophisticated analysis, with
       * append_callgraph_to_prolog_db 
       *)
      let file ="
class A { function foo() { } }
class B extends A { }
function bar() {
  $o = new B();
  $y = $o->foo();
} " in
      let xs = prolog_query ~file 
        "docall2('bar', (X,Y), method), writeln((X,Y)), fail" in
      assert_equal ~msg:"it should find basic callers to a function"
        ["A,foo"]
        (sort xs);
      

    );

    (*-----------------------------------------------------------------------*)
    (* Exceptions *)
    (*-----------------------------------------------------------------------*)
    "exceptions" >:: (fun () ->
      let file = "
class Exception { }
class ViolationException extends Exception { }
class ForgotExtendsException { }
class UnrelatedClass { }

function foo() { 
  throw new Exception(); 
}
function bar() { 
  try { 
    throw new ViolationException();
  } catch (ViolationException $e) {
  }
}
function bad() { 
  throw new ForgotExtendsException(); 
}
" in
      let xs = prolog_query ~file "throw('foo', X), writeln(X)" in
      assert_equal ~msg:"it should find basic throw"
        ["Exception"]
        xs;
      let xs = prolog_query ~file "catch('bar', X), writeln(X)" in
      assert_equal ~msg:"it should find basic catch"
        ["ViolationException"]
        xs;
      let xs = prolog_query ~file 
        ("throw(_, X), not(children(X, 'Exception')), X \\= 'Exception', " ^ 
        "writeln(X)") in
      assert_equal ~msg:"it should find exceptions not deriving from Exception"
        ["ForgotExtendsException"]
        xs;
    );

    (*-----------------------------------------------------------------------*)
    (* Data graph *)
    (*-----------------------------------------------------------------------*)
    "arrays used as records" >:: (fun () ->
      let file = "
function foo($x) {
  echo $x['bar'];
}
" in
    let xs = prolog_query ~file "use(X, 'bar', array, read), writeln(X)" in
    assert_equal ~msg:"it should find read accesses to a record field"
      ["foo"] (xs);
    );

    "fields use" >:: (fun () ->
      let file = "
class A {
 public $bar = 0;
}
function foo(A $o) {
  echo $o->bar;
}
" in
    let xs = prolog_query ~file "use(X, 'bar', field, read), writeln(X)" in
    assert_equal ~msg:"it should find read accesses to an object field"
      ["foo"] (xs);
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
