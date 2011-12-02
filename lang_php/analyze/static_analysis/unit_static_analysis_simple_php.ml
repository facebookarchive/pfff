open Common

open OUnit

module Db = Database_php
module Cg = Callgraph_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let db_from_string s =
  let tmp_file = Parse_php.tmp_php_file_from_string s in
  (* make sure it's a valid PHP file *)
  let _ast = Parse_php.parse_program tmp_file in
  Database_php_build.db_of_files_or_dirs [tmp_file]

(* A few shortcuts to make our testing code more compact and declarative.
 * Allow for instance to get the id of a class by using the special :: syntax
 * as in (id "A::" db):
 *  - X:: for a class
 *  - X::Y for a method
 *  - Anything else for a function
 *)
let id s db =
  match s with
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
(* 
 * The deadcode analysis in pfff we do for facebook does not only find
 * dead code; It also:
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

        let db = Database_php_build.db_of_files_or_dirs [deadcode_data_dir] in
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

        let db = Database_php_build.db_of_files_or_dirs [deadcode_data_dir] in
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

        (* TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
        OUnit.skip_if true "need have cleaner codebase so no false positif";
        let db = Database_php_build.db_of_files_or_dirs [deadcode_data_dir] in
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
        let db = Database_php_build.db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in

        (* TODO !!!!!!!!!!!!!!!!!!!!!!!!! *)
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

      (* todo: dead methods (quite hard), actually we already have
       * troubles with dead static methods ... hmmm
       * 
       * less: dead defines 
       *)
    ])

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)
let unittest =
  "static_analyze_php" >::: [
    deadcode_unittest;
  ]
