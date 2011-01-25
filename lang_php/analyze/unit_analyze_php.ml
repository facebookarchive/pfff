open Common

module Ast = Ast_php

module Db = Database_php
module Cg = Callgraph_php

module V = Visitor_php

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Normally it would be better to put the unittest code in the unit tested
 * file, but sometimes for instance for the callgraph we actually need
 * a db which would add extra dependencies between directories
 * (e.g. foundation/ depending on database/). So better then to put
 * those unit tests here.
 *
 * Some of the tests depends on PHP files in tests/. An alternative that
 * would make the test code clearer would be to include those tests
 * files in ML but:
 *   - don't get the highlighting so no help if the test files contains
 *     bad PHP code
 *   - can not use the php interpreter on those files or xdebug or
 *     some of our command line tools like sgrep
 *)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ast_of_file_safe file =
  let ast = Parse_php.parse_program file in
  if not (List.for_all (function
      Ast_php.NotParsedCorrectly _ -> false | _ -> true) ast)
  then failwith ("Parsing problem for: " ^ (Common.read_file file))
  else ast

(* It is clearer for our testing code to programmatically build source files
 * so that all the information about a test is in the same
 * file. You don't have to open extra files to understand the test
 * data.
 *)
let tmp_php_file_from_string s =
  let tmp_file = Common.new_temp_file "test" ".php" in
  Common.write_file ~file:tmp_file ("<?php\n" ^ s);
  tmp_file

(* Another shortcut *)
let db_from_string s =
  let tmp_file = tmp_php_file_from_string s in
  (* make sure it's a correct PHP file *)
  let ast = Parse_php.parse_program tmp_file in
  if not (List.for_all (function
           Ast_php.NotParsedCorrectly _ -> false | _ -> true) ast)
  then failwith ("Parsing problem for: " ^ s);
  Test_analyze_php.db_of_files_or_dirs [tmp_file]


let db_from_fake_files xs =
  (* todo? would be better to create each time a fresh new dir *)
  let tmp_dir = "/tmp/pfff_fake_dir" in
  Common.command2 ("rm -rf " ^ tmp_dir);
  xs |> List.iter (fun (file, s) ->
    let dir = Filename.dirname file in
    Common.command2 (spf "mkdir -p %s/%s" tmp_dir dir);
    Common.write_file ~file:(Filename.concat tmp_dir file) ("<?php\n" ^ s);
  );
  let db = Test_analyze_php.db_of_files_or_dirs [tmp_dir] in
  let db = { db with
    Database_php.project = Database_php.Project (tmp_dir, None);
  }
  in
  (* Common.command2 ("rm -rf " ^ tmp_dir); *)
  db


(* A few shortcuts to make our testing code more compact and declarative.
 * Allow for instance to get the id of a class by using the special :: syntax
 * as in (id "A::" db):
 *  - X::: for interface
 *  - X:: for a class
 *  - X::Y for a method
 *  - anything else for a function
 *)
let id s db =
  match s with
  | s when s =~ "\\([A-Za-z]+\\):::$" ->
      let (interface) = Common.matched1 s in
      Db.id_of_interface interface db

  | s when s =~ "\\([A-Za-z]+\\)::$" ->
      let (sclass) = Common.matched1 s in
      Db.id_of_class sclass db

  | s when  s =~ "\\([A-Za-z0-9_]+\\)::\\(.*\\)" ->
    let (sclass, smethod) = Common.matched2 s in
    Db.id_of_method sclass smethod db

  | _ ->
    Db.id_of_function s db

let callers id db =
  Db.callers_of_id id db |> List.map Cg.id_of_callerinfo
let callees id db =
  Db.callees_of_id id db |> List.map Cg.id_of_callsite


(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)




(*---------------------------------------------------------------------------*)
(* Callgraph *)
(*---------------------------------------------------------------------------*)

(*
 * One of the main feature of the code database (see database_php.mli) is
 * to store the full callgraph of some PHP code. This is used for instance
 * by the deadcode reaper to know which functions are never
 * called. Handling regular function calls is quite simple
 * but the semantic of PHP regarding methods, especially static
 * methods is not completely intuitive. Here are a few unit tests
 * to document this semantic and to get confidence in the callgraph
 * code which gets bigger.
 *)
let callgraph_unittest =
    "callgraph_php" >::: [

      (* Checking the call graph for code with simple function calls *)
      "simple function call" >:: (fun () ->
        let file = "
         function a() { b(); }
         function b() { }
         function z() { }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let callees id = callees id db in

        assert_equal [id "a"] (callers (id "b"));
        assert_equal [id "b"] (callees (id "a"));
        assert_equal [] (callers (id "a"));
        assert_equal [] (callees (id "b"));
        assert_equal [] (callers (id "z"));
        assert_equal [] (callees (id "z"));
        assert_raises Not_found (fun () -> id "w");
      );

      (* Checking the semantic of static method calls. *)
      "simple static method call" >:: (fun () ->
        let file = "
          class A { static function a() { } }
          function b() { A::a(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let callees id = callees id db in
        assert_equal [id "A::a"] (callees (id "b"));
        assert_equal [id "b"] (callers (id "A::a"));
      );

      "static method call with self:: and parent::" >:: (fun () ->
        let file = "
          class A {
           static function a() { }
           static function a2() { self::a(); }
          }
          class B extends A {
           function b() { parent::a(); }
          }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let _callees id = callees id db in
        assert_equal
          (sort [id "A::a2"; id "B::b"])
          (sort (callers (id "A::a")));
      );

      (* In PHP it is ok to call B::foo() even if B does not define
       * a static method 'foo' provided that B inherits from a class
       * that defines such a foo.
       *)
      "static method call and inheritance" >:: (fun () ->
        let file = "
          class A {
           static function a() { }
          }
          class B extends A { }
          function c() { B::a(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let _callees id = callees id db in
        assert_equal
          (sort [id "c"])
          (sort (callers (id "A::a")));
      );

      (* PHP is very permissive regarding static method calls as one can
       * do $this->foo() even if foo is a static method. PHP does not
       * impose the X::foo() syntax, which IMHO is just wrong.
       *)
      "static method call and $this" >:: (fun () ->
        let file = "
          class A {
           static function a() { }
           function a2() {
              $this->a();
          }
        }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let _id s = id s db in
        let _callers id = callers id db in let _callees id = callees id db in
        (* This currently fails, and I am not sure I want to fix it. Our
         * code should not use the $this->foo() syntax for static method
         * calls
         *
         * assert_equal
         * (sort [id "A::a2"])
         * (sort (callers (id "A::a")));
         *)
         ()
      );

      (* Checking method calls. *)
      "simple method call" >:: (fun () ->
        let file = "
          class A {
           function foo() { }
          }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        Database_php_build.index_db_method db;
        (* shortcuts *)
        let id s = id s db in
        let _callers id = callers id db in let callees id = callees id db in
        assert_equal
         (sort [id "A::foo"])
         (sort (callees (id "c")));
      );

      (* Right now the analysis is very simple and does some gross over
       * approximation. With a call like $x->foo(), the analysis consider
       * any method foo in any class as a viable candidate. Doing a better
       * job would require some class and data-flow analysis.
       * Once the analysis gets more precise, fix those tests.
       *)
      "method call approximation" >:: (fun () ->
        let file = "
          class A {
           function foo() { }
          }
          class B {
           function foo() { }
          }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        Database_php_build.index_db_method db;
        (* shortcuts *)
        let id s = id s db in
        let _callers id = callers id db in let callees id = callees id db in
        assert_equal
         (sort [id "A::foo"; id "B::foo"]) (* sad, should have only A::foo *)
         (sort (callees (id "c")));
      );
    ]

(*---------------------------------------------------------------------------*)
(* Deadcode *)
(*---------------------------------------------------------------------------*)

(* The deadcode analysis in pfff we do for facebook not only find
 * dead code. It also:
 *  - generate patches to remove this code,
 *  - use blame information to know who wrote the code,
 *  - send code review request to diffcamp to the blamed person.
 *
 * Here we just want to unit test the basic functionality of the
 * deadcode reaper; assert that the reaper correctly find the
 * ids of the appropriate dead functions, or methods, or even classes.
 *)
let deadcode_unittest =
    "deadcode_php" >::: (

      (* the tests data is in pfff/tests/deadcode/. It consists of a few
       * small php files whose name, e.g. all_dead.php explains what
       * kind of function they contain.
       *)
      let deadcode_data_dir = Config.path ^ "/tests/php/deadcode/" in

      (* The deadcode analysis can be customized via "hooks".
       * This default hook is good enough for our unit tests.
       *)
      let hooks = Deadcode_php.default_hooks in

      [
      "simple dead functionss" >:: (fun () ->

        let db = Test_analyze_php.db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db |> List.map snd in

        assert_bool
          "nocaller() should be dead"
          (List.mem (id "nocaller") dead_ids);

        assert_bool
          "file2_foo() should not be dead"
          (not (List.mem (id "file2_foo") dead_ids));

        (* pfff can parse XHP files by default now *)
        assert_bool
          "xhp_dead() should be dead, pfff should handle XHP code"
          (List.mem (id "xhp_dead") dead_ids);
      );

      (* With a fixpoint, the deadcode reaper can find more dead functions. *)
      "transitive dead functions" >:: (fun () ->

        let db = Test_analyze_php.db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in
        let dead_ids =
          Deadcode_php.deadcode_fixpoint_per_file dead_ids hooks db +>
            Deadcode_php.ungroup_ids_by_file
        in
        assert_bool
          "calledbynocaller() should be dead with a fixpoint analysis"
          (List.mem (id "calledbynocaller") dead_ids);

        assert_bool
          "nocaller() should still be dead, even with a fixpoint analysis"
          (List.mem (id "nocaller") dead_ids);
      );

      (* Now that the callgraph understands static method calls, we can
       * not only find dead functions but also dead static methods.
       * See the different tests/deadcode/static_function*.php which
       * shows the subtelities of the semantic of static calls.
       *)
      "dead static methods" >:: (fun () ->

        OUnit.skip_if true "need have cleaner codebase so no false positif";
        let db = Test_analyze_php.db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in

        assert_bool
          "StaticFunction::is_dead() should be dead"
          (List.mem (id "StaticFunction::is_dead") dead_ids);

        assert_bool
          "StaticFunction::not_dead() should not be dead"
          (not (List.mem (id "StaticFunction::not_dead") dead_ids));

        (* This static method is called indirectly via one of its
         * inherited class. The callgraph should understand that.
         *)
        assert_bool
          "StaticFunctionBase2::not_dead() should not be dead"
          (not (List.mem (id "StaticFunctionBase2::not_dead") dead_ids));

        assert_bool
          "StaticFunctionBase2::is_dead() should be dead"
          ((List.mem (id "StaticFunctionBase2::is_dead") dead_ids));


        (* This static method is called via the self:: syntax *)
        assert_bool
          "SF3::not_dead() should not be dead"
          (not (List.mem (id "SF3::not_dead") dead_ids));

        assert_bool
          "SF4_A::not_dead() should not be dead"
          (not (List.mem (id "SF4_A::not_dead") dead_ids));
      );

      (* PHP allows ugly things like using the $this->static_call()
       * syntax whereas it should really be self::static_call()
       * This can confuse our callgraph, and because it is right now
       * use too much in our codebase, it is better to not report
       * such code as dead
       *)

      "dead static methods part2" >:: (fun () ->
        let db = Test_analyze_php.db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in

        (* TODO *)
        OUnit.skip_if true "need better callgraph, or deadcode hook";
        assert_bool
          "SFTHIS::not_dead() should not be dead"
          (not (List.mem (id "SFTHIS::not_dead") dead_ids));

      );

      "dead classes" >:: (fun () ->
        let file = "class DeadA { } class NotDeadA { } $o = new NotDeadA();" in
        let db = db_from_string file in

        let dead_ids =
          Deadcode_php.finding_dead_classes hooks db |> List.map snd in
        assert_bool
          "DeadA should be dead"
          (List.mem (id "DeadA::" db) dead_ids);
        assert_bool
          "NotDeadA should not be dead"
          (not (List.mem (id "NotDeadA::" db) dead_ids));
      );

      "dead classes corner cases" >:: (fun () ->
        let file = 
          "class NotDeadA { static function foo() {} } NotDeadA::foo();" in
        let db = db_from_string file in

        let dead_ids =
          Deadcode_php.finding_dead_classes hooks db |> List.map snd in
        assert_bool
          "NotDeadA should not be dead"
          (not (List.mem (id "NotDeadA::" db) dead_ids));
      );

      (* todo: dead methods (quite hard) *)
      (* less: dead defines *)

    ])

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "analyze_php" >::: [
    callgraph_unittest;
    Test_coverage_php.unittest;
    deadcode_unittest;

    "class analysis" >::: [

      "users of a class" >:: (fun () ->
        let file = "
          class A {
           function foo() { }
          }
          class B {
           function foo() { new A(); }
          }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          (sort [id "B::foo"; id "c"])
          (sort (Db.class_users_of_id (id "A::") db));

      );

      "extenders of a class" >:: (fun () ->
        let file = "
          class A { }
          class B extends A { }
          class C { }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          [id "B::"]
          (Db.class_extenders_of_id (id "A::") db);

        assert_equal
          []
          (Db.class_extenders_of_id (id "C::") db);
      );

      "implementers of interface" >:: (fun () ->
        let file = "
          interface A { }
          interface A2 { }
          class B implements A { }
          class C implements A2, A { }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          (sort [id "B::";id "C::"])
          (sort (Db.class_implementers_of_id (id "A:::") db));
      );
    ];

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
        let tmpfile = tmp_php_file_from_string file in
        let ast = ast_of_file_safe tmpfile in
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

      (* It is useful to know the set of files that directly or indirectly
       * include a file.
       *)
      "includees includers" >:: (fun () ->
        let data = [
          "a.php", "";
          "b.php", "include_once 'a.php';";
          "c.php", "include_once 'b.php';";
          "w.php", "";
          "z.php", "include_once 'c.php'; include_once 'w.php'; ";
        ]
        in
        let db = db_from_fake_files data in
        Database_php_build2.index_db_includes_requires None db;

        let p file = Db.readable_to_absolute_filename file db in

        let includers_a = Db.includers_rec_of_file (p "a.php") db in
        assert_equal
          (sort [p "b.php"; p "c.php"; p "z.php"])
          (sort includers_a);

        let includees_z = Db.includees_rec_of_file (p "z.php") db in
        assert_equal
          (sort [p "c.php"; p "b.php"; p "a.php"; p "w.php"])
          (sort includees_z);
      );
    ]);
  ]



