open Common

open Ast_php

module Db = Database_php
module Cg = Callgraph_php

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Some tests depend on PHP files under tests/. Some tests are
 * included inline here as strings. There are pros and cons for both.
 * For large test files it's better to have them under tests/ otherwise:
 *   - you don't get the highlighting so no help if the test file contains
 *     bad PHP code
 *   - you can not use the php interpreter on those files or xdebug or
 *     some of our command line tools that we also want to test like sgrep
 * 
 * todo: remove the callgraph from database_php.ml and port the unit
 * tests below to unit_prolog_php.ml.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let db_from_string s =
  let tmp_file = Parse_php.tmp_php_file_from_string s in
  (* make sure it's a valid PHP file *)
  let _ast = Parse_php.parse_program tmp_file in
  Database_php_build.db_of_files_or_dirs [tmp_file]

let entity_finder_from_string s =
  let db = db_from_string s in
  Database_php_build.build_entity_finder db

let db_from_fake_files xs =
  (* todo? would be better to create each time a fresh new dir *)
  let tmp_dir = "/tmp/pfff_fake_dir" in
  Common.command2 ("rm -rf " ^ tmp_dir);
  xs |> List.iter (fun (file, s) ->
    let dir = Filename.dirname file in
    Common.command2 (spf "mkdir -p %s/%s" tmp_dir dir);
    Common.write_file ~file:(Filename.concat tmp_dir file) ("<?php\n" ^ s);
  );
  let db = Database_php_build.db_of_files_or_dirs [tmp_dir] in
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
(* Database building *)
(*---------------------------------------------------------------------------*)

let database_unittest =
  "database_php" >::: [

    "simple database" >:: (fun () ->
      let data_dir = Config_pfff.path ^ "/tests/php/parsing/" in
      (* less: should put true here and fix tests/php/parsing *)
      Common.save_excursion Flag_analyze_php.show_errors false (fun () ->
      let _db = Database_php_build.db_of_files_or_dirs [data_dir] in
      ()
      )
    );
    "accept files with parse error" >:: (fun () ->
      let data_dir = Config_pfff.path ^ "/tests/php/parsing_errors/" in
      Common.save_excursion Flag_analyze_php.show_errors false (fun () ->
      Common.save_excursion Flag_analyze_php.verbose_database false (fun () ->
      Common.save_excursion Flag_parsing_php.verbose_lexing false (fun () ->
      Common.save_excursion Flag_parsing_php.verbose_parsing false (fun () ->
      Common.save_excursion Common.verbose_level 0 (fun () ->
      let _db = Database_php_build.db_of_files_or_dirs [data_dir] in
      ()
      )))))
    );
    "light database" >:: (fun () ->
      let data_dir = Config_pfff.path ^ "/tests/php/db/" in
      let db = Database_php_build.db_of_files_or_dirs [data_dir] in
      let _dbcode = 
        Database_light_php.database_code_from_php_database
          ~verbose:false db
      in
      ()
        
    );

  ]


(*---------------------------------------------------------------------------*)
(* Functions use/def, callgraph *)
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
 * 
 * todo: this is now less useful because julien's callgraph is actually
 * more precise. We should really just extend prolog's database with
 * the precise callgraph using the abstract interpreter and then
 * have unit tests only on prolog's database.
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
    ]

(*---------------------------------------------------------------------------*)
(* Classes use/def *)
(*---------------------------------------------------------------------------*)

(* todo: port this to prolog *)
let class_unittest =
    "class analysis" >::: [

      "users of a class" >:: (fun () ->
        let file = "
          class A { function foo() { } }
          class B { function foo() { new A(); } }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          (sort [id "B::foo"; id "c"; id "B::"])
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
(*
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
*)
    ]

(*---------------------------------------------------------------------------*)
(* Inheritance semantic *)
(*---------------------------------------------------------------------------*)
let lookup_unittest =
  "lookup method php" >::: [

    "static lookup" >:: (fun () ->
      let file = "
class A { static function a() { return CST; } }
" in
      let find_entity = entity_finder_from_string file in
      let def = Class_php.lookup_method ("A","a") find_entity in
      match def with
      | { f_body = (_, 
         [(Return (_, (Some (Sc (C (CName (Name ("CST",_)))))), _))], _); _ }
          -> ()
      | _ ->assert_failure "it should find simple static method"
    );

    "static method recursive lookup" >:: (fun () ->
      let file = "
class A { static function a() { return CST; } }
class B extends A { }
" in
      let find_entity = entity_finder_from_string file in
      let def = Class_php.lookup_method ("B","a") find_entity in
      match def with
      | { f_body = (_,
         [(Return (_, (Some (Sc (C (CName (Name ("CST",_)))))), _))], _); _ }
          -> ()
      | _ ->assert_failure "it should find static method in parent class"
    );

    "method lookup and traits" >:: (fun () ->
      let file = "
trait T { static function t() { return CST; } }
class A { use T; }
class B extends A { }
" in
      let find_entity = entity_finder_from_string file in
      let def = Class_php.lookup_method ("B","t") find_entity in
      match def with
      | { f_body = (_, 
         [(Return (_, (Some (Sc (C (CName (Name ("CST",_)))))), _))], _); _ }
          -> ()
      | _ ->assert_failure "it should find static method in mixin trait"
    );

    (* TODO: works with xhp classes? *)
  ]

(*---------------------------------------------------------------------------*)
(* Include use/def *)
(*---------------------------------------------------------------------------*)

(* see also unit_foundation_php.ml *)
let include_unittest =
  "include_require_db" >::: [
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
        Common.save_excursion Flag_analyze_php.verbose_database false (fun()->
          Database_php_build2.index_db_includes_requires None db
        );

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
    ]

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "analyze_db_php" >::: [
    database_unittest;
    callgraph_unittest;
    class_unittest;
    lookup_unittest;
    include_unittest;
    (* now in static_analysis/: deadcode_unittest; *)
    (* now in checker/: checkers_unittest; *)
  ]
